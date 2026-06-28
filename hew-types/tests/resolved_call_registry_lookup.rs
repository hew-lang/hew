//! W4.001 Stage A substrate test: `ImplRegistry` lookup + bound satisfaction.
//!
//! This is the "substrate-tests-the-substrate" test for the new dispatch
//! authority (`hew-types/src/check/dispatch.rs`). It seeds an
//! [`ImplRegistry`] with representative impls (`HashMap`, `HashSet`, `Vec`) and
//! exercises [`resolve_method_call`] over the call shapes Stage B's
//! resolver will encounter — covering:
//!
//! 1. **Happy-path lookup**: receiver matches a registered impl, all
//!    where-bounds satisfied, target method present → returns the
//!    expected [`ResolvedCall`] with the right `impl_id`, target ABI,
//!    and concrete `type_args`.
//! 2. **Bound-satisfaction failure**: receiver matches structurally
//!    but a where-bound (e.g. `K: Hash`) is rejected by the
//!    caller-supplied predicate → returns
//!    [`LookupError::BoundsNotSatisfied`] naming the offending bound
//!    and a witness pattern. Bounds are **evaluated**, not bypassed.
//! 3. **Structural miss**: no impl matches the receiver shape →
//!    [`LookupError::NoImpl`].
//! 4. **Unknown method**: impl matches and bounds satisfied, method
//!    name absent → [`LookupError::UnknownMethod`].
//! 5. **Serde round-trip**: the entire seeded registry encodes to JSON
//!    and decodes back to an identical registry. This is the
//!    structural enforcement of the Q281=A data-only discipline — a
//!    `Box<dyn Fn>` or closure smuggled into `ImplDef` would not be
//!    serialisable and this round-trip would fail to compile or fail
//!    at runtime. Per §7 risk #3 of the design notes.
//!
//! This test does NOT touch `TypeCheckOutput`. Stage A's invariant is
//! that no production reader of `resolved_calls` exists; the field is
//! exercised here purely against the dispatch substrate.

use hew_types::check::collection_dispatch_registry_for_tests;
use hew_types::check::dispatch::{
    match_pattern, resolve_method_call, Bound, CallAbiHint, HashMapMethod, HashSetMethod, ImplDef,
    ImplId, ImplRegistry, LookupError, MethodTarget, MethodTargetFamily, RuntimeAbi, TyPattern,
    VecMethod,
};
use hew_types::traits::MarkerTrait;
use std::collections::BTreeSet;

// ---- Test fixtures ---------------------------------------------------------

fn primitive(name: &str) -> TyPattern {
    TyPattern::Primitive(name.to_string())
}

fn app(ctor: &str, args: Vec<TyPattern>) -> TyPattern {
    TyPattern::App {
        ctor: ctor.to_string(),
        args,
    }
}

fn var(name: &str) -> TyPattern {
    TyPattern::Var(name.to_string())
}

/// Mirrors the canonical `K: Hash + Eq` admission policy that Stage C will
/// fold into the resolver's authoritative predicate. Kept inside the test
/// (not on `ImplDef`) per Q281=A.
fn hash_eq_satisfied(trait_name: MarkerTrait, pat: &TyPattern) -> bool {
    let pat = match pat {
        TyPattern::Primitive(name) => name.as_str(),
        // For App / Tuple / Var we conservatively reject every marker —
        // Stage A's audit covers the primitives; Stage B will deepen this
        // to recurse into composite shapes via `TraitRegistry`.
        _ => return false,
    };
    match (trait_name, pat) {
        (MarkerTrait::Hash | MarkerTrait::Eq, p) => matches!(
            p,
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
        // No other markers in scope for the Stage A test (Send/Sync/etc.
        // are exercised by `traits::tests`).
        _ => false,
    }
}

fn seed_registry() -> (ImplRegistry, ImplId, ImplId, ImplId) {
    let mut r = ImplRegistry::new();
    // impl<K: Hash + Eq, V> Map for HashMap<K, V>
    let map_id = r.register(ImplDef {
        trait_name: "Map".into(),
        self_pattern: app("HashMap", vec![var("K"), var("V")]),
        where_bounds: vec![
            Bound {
                trait_name: MarkerTrait::Hash,
                var: "K".into(),
            },
            Bound {
                trait_name: MarkerTrait::Eq,
                var: "K".into(),
            },
        ],
        methods: vec![
            (
                "insert".into(),
                MethodTarget {
                    symbol_name: "hew_hashmap_insert".into(),
                    family: MethodTargetFamily::HashMap(HashMapMethod::Insert),
                    abi: RuntimeAbi::ByRefMut,
                    call_hint: CallAbiHint::RuntimeShim,
                    consumes_receiver: false,
                },
            ),
            (
                "get".into(),
                MethodTarget {
                    symbol_name: "hew_hashmap_get".into(),
                    family: MethodTargetFamily::HashMap(HashMapMethod::Get),
                    abi: RuntimeAbi::ByRef,
                    call_hint: CallAbiHint::RuntimeShim,
                    consumes_receiver: false,
                },
            ),
        ],
    });
    // impl<T: Hash + Eq> Set for HashSet<T>
    let set_id = r.register(ImplDef {
        trait_name: "Set".into(),
        self_pattern: app("HashSet", vec![var("T")]),
        where_bounds: vec![
            Bound {
                trait_name: MarkerTrait::Hash,
                var: "T".into(),
            },
            Bound {
                trait_name: MarkerTrait::Eq,
                var: "T".into(),
            },
        ],
        methods: vec![(
            "insert".into(),
            MethodTarget {
                symbol_name: "hew_hashset_insert".into(),
                family: MethodTargetFamily::HashSet(HashSetMethod::Insert),
                abi: RuntimeAbi::ByRefMut,
                call_hint: CallAbiHint::RuntimeShim,
                consumes_receiver: false,
            },
        )],
    });
    // impl<T> Vec for Vec<T>  -- no bounds
    let vec_id = r.register(ImplDef {
        trait_name: "Vec".into(),
        self_pattern: app("Vec", vec![var("T")]),
        where_bounds: vec![],
        methods: vec![(
            "push".into(),
            MethodTarget {
                symbol_name: "hew_vec_push".into(),
                family: MethodTargetFamily::Vec(VecMethod::Push),
                abi: RuntimeAbi::ByRefMut,
                call_hint: CallAbiHint::Direct,
                consumes_receiver: false,
            },
        )],
    });
    (r, map_id, set_id, vec_id)
}

// ---- Tests -----------------------------------------------------------------

#[test]
fn hashmap_string_i64_insert_resolves_to_map_impl() {
    let (r, map_id, _, _) = seed_registry();
    let receiver = app("HashMap", vec![primitive("String"), primitive("i64")]);
    let resolved = resolve_method_call(&r, "Map", "insert", &receiver, &hash_eq_satisfied)
        .expect("HashMap<String,i64>::insert must resolve");
    assert_eq!(resolved.impl_id, map_id);
    assert_eq!(resolved.method_name, "insert");
    assert_eq!(resolved.target.symbol_name, "hew_hashmap_insert");
    assert_eq!(resolved.target.abi, RuntimeAbi::ByRefMut);
    assert_eq!(resolved.target.call_hint, CallAbiHint::RuntimeShim);
    assert!(!resolved.target.consumes_receiver);
    // type_args follow first-occurrence order of vars in self_pattern: K, V.
    assert_eq!(
        resolved.type_args,
        vec![primitive("String"), primitive("i64")]
    );
}

#[test]
fn hashmap_i64_key_resolves_when_i64_satisfies_hash_eq() {
    // Stage A's policy already accepts i64 as Hash + Eq; verifies the
    // resolver evaluates bounds rather than gating on a separate
    // per-K-class allowlist (DI-008 prep — see plan §3 Stage A).
    let (r, map_id, _, _) = seed_registry();
    let receiver = app("HashMap", vec![primitive("i64"), primitive("i64")]);
    let resolved = resolve_method_call(&r, "Map", "insert", &receiver, &hash_eq_satisfied)
        .expect("HashMap<i64,i64>::insert must resolve under Hash+Eq bounds");
    assert_eq!(resolved.impl_id, map_id);
}

#[test]
fn hashmap_f64_key_rejected_with_bounds_not_satisfied() {
    let (r, map_id, _, _) = seed_registry();
    let receiver = app("HashMap", vec![primitive("f64"), primitive("i64")]);
    let err = resolve_method_call(&r, "Map", "insert", &receiver, &hash_eq_satisfied)
        .expect_err("HashMap<f64,i64>::insert must reject — f64 not Hash");
    match err {
        LookupError::BoundsNotSatisfied {
            impl_id,
            unsatisfied,
            witness,
        } => {
            assert_eq!(impl_id, map_id);
            // Both Hash and Eq are unsatisfied for f64.
            let traits: Vec<MarkerTrait> = unsatisfied.iter().map(|b| b.trait_name).collect();
            assert!(traits.contains(&MarkerTrait::Hash));
            assert!(traits.contains(&MarkerTrait::Eq));
            assert!(unsatisfied.iter().all(|b| b.var == "K"));
            assert_eq!(witness, primitive("f64"));
        }
        other => panic!("expected BoundsNotSatisfied, got {other:?}"),
    }
}

#[test]
fn hashset_i64_insert_resolves() {
    let (r, _, set_id, _) = seed_registry();
    let receiver = app("HashSet", vec![primitive("i64")]);
    let resolved = resolve_method_call(&r, "Set", "insert", &receiver, &hash_eq_satisfied)
        .expect("HashSet<i64>::insert must resolve");
    assert_eq!(resolved.impl_id, set_id);
    assert_eq!(resolved.type_args, vec![primitive("i64")]);
}

#[test]
fn vec_push_resolves_with_no_bounds() {
    let (r, _, _, vec_id) = seed_registry();
    let receiver = app("Vec", vec![primitive("i64")]);
    let resolved = resolve_method_call(&r, "Vec", "push", &receiver, &hash_eq_satisfied)
        .expect("Vec<i64>::push must resolve (no where-bounds)");
    assert_eq!(resolved.impl_id, vec_id);
    assert_eq!(resolved.target.symbol_name, "hew_vec_push");
    assert_eq!(resolved.target.call_hint, CallAbiHint::Direct);
}

#[test]
fn unknown_trait_returns_no_impl() {
    let (r, _, _, _) = seed_registry();
    let receiver = app("BTreeMap", vec![primitive("i64"), primitive("i64")]);
    let err = resolve_method_call(&r, "Map", "insert", &receiver, &hash_eq_satisfied)
        .expect_err("BTreeMap is not in the seeded registry");
    assert!(matches!(err, LookupError::NoImpl { .. }));
}

#[test]
fn unknown_method_on_matching_impl_is_distinguished() {
    let (r, map_id, _, _) = seed_registry();
    let receiver = app("HashMap", vec![primitive("String"), primitive("i64")]);
    let err = resolve_method_call(&r, "Map", "no_such_method", &receiver, &hash_eq_satisfied)
        .expect_err("Map impl has no `no_such_method`");
    match err {
        LookupError::UnknownMethod { impl_id, method } => {
            assert_eq!(impl_id, map_id);
            assert_eq!(method, "no_such_method");
        }
        other => panic!("expected UnknownMethod, got {other:?}"),
    }
}

#[test]
fn match_pattern_round_trip_via_dispatch_lookup() {
    // Spot-check the matcher independently — the resolver uses it but a
    // bare matcher test guards against silent semantic drift.
    let pat = app("HashMap", vec![var("K"), var("V")]);
    let concrete = app("HashMap", vec![primitive("i32"), primitive("bool")]);
    let subst = match_pattern(&pat, &concrete).expect("must match");
    assert_eq!(subst.get("K"), Some(&primitive("i32")));
    assert_eq!(subst.get("V"), Some(&primitive("bool")));
}

#[test]
fn registry_round_trips_via_serde_json_proving_data_only_shape() {
    // The structural enforcement of Q281=A data-only discipline. A
    // `Box<dyn Fn>` in ImplDef / TyPattern / Bound / MethodTarget would
    // not implement Serialize, so this test would fail to compile.
    // Adding the test ensures the discipline cannot regress silently.
    let (r, _, _, _) = seed_registry();
    let json = serde_json::to_string(&r).expect("registry must serialize to JSON");
    let decoded: ImplRegistry =
        serde_json::from_str(&json).expect("registry must round-trip from JSON");
    assert_eq!(r, decoded);

    // Also round-trip a successfully resolved call, since `ResolvedCall`
    // is what populates `TypeCheckOutput::resolved_calls` in Stage B.
    let receiver = app("HashSet", vec![primitive("char")]);
    let resolved = resolve_method_call(&r, "Set", "insert", &receiver, &hash_eq_satisfied)
        .expect("HashSet<char> resolves");
    let resolved_json = serde_json::to_string(&resolved).expect("ResolvedCall serializes");
    let decoded_resolved: hew_types::check::dispatch::ResolvedCall =
        serde_json::from_str(&resolved_json).expect("ResolvedCall round-trips");
    assert_eq!(resolved, decoded_resolved);
}

#[test]
fn production_registry_exposes_vec_seq_methods_with_one_type_arg() {
    let registry = collection_dispatch_registry_for_tests();
    let (seq_id, seq_impl) = registry
        .iter()
        .find(|(_, def)| def.trait_name == "Seq")
        .expect("collection registry must expose the Vec-backed Seq impl");

    assert_eq!(
        seq_impl.self_pattern,
        app("Vec", vec![var("T")]),
        "Seq impl must bind exactly Vec<T>"
    );
    assert!(
        seq_impl.where_bounds.is_empty(),
        "Vec dispatch has no registry where-bounds; per-element gates stay in the Vec symbol router"
    );

    let methods: BTreeSet<&str> = seq_impl
        .methods
        .iter()
        .map(|(method, _)| method.as_str())
        .collect();
    let expected = BTreeSet::from([
        "push", "pop", "len", "get", "set", "remove", "contains", "is_empty", "clear", "clone",
        "append", "join",
    ]);
    assert_eq!(methods, expected);

    let receiver = app("Vec", vec![primitive("i64")]);
    for (method, target) in &seq_impl.methods {
        let resolved = resolve_method_call(&registry, "Seq", method, &receiver, &hash_eq_satisfied)
            .unwrap_or_else(|err| panic!("Seq::{method} rejected for Vec<i64>: {err:?}"));
        assert_eq!(resolved.impl_id, seq_id);
        assert_eq!(
            resolved.type_args,
            vec![primitive("i64")],
            "Seq::{method} must carry the single Vec element type arg"
        );
        assert!(
            resolved.target.symbol_name.starts_with("hew_vec_"),
            "Seq::{method} placeholder must stay in the hew_vec_* family"
        );
        assert!(
            resolved.target.symbol_name.ends_with("_FAMILY"),
            "Seq::{method} registry symbol is a placeholder until Vec's per-element override runs"
        );
        assert_eq!(resolved.target.abi, target.abi);
        assert_eq!(resolved.target.call_hint, CallAbiHint::RuntimeShim);
        assert!(!resolved.target.consumes_receiver);
    }
}
