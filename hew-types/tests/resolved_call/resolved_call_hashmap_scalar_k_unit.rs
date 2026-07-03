//! W4.001 Stage C0b — unit-level resolver output verification.
//!
//! Given a `HashMap<K, V>` receiver shape where `K` is any scalar / string
//! / bytes type the kernel admits as a layout-key, the Stage B
//! `collection_dispatch_registry` resolver must return a
//! [`MethodTarget`] whose `symbol_name` is the canonical kernel symbol
//! (`hew_hashmap_<method>_layout`).
//!
//! **Does NOT assert checker-accept of the surrounding user-facing call.**
//! C0b deliberately does not widen the checker's admissibility allowlists
//! — those retirements land at Stage C in the DI-017 combined-commit.
//! This test exercises the *resolver* in isolation against the receiver
//! shape, which is the unit Stage C will plug into.
//!
//! Per plan §4 Stage C0b and brief §"Mandatory tests".

use hew_types::check::collection_dispatch_registry_for_tests;
use hew_types::check::dispatch::{resolve_method_call, CallAbiHint, RuntimeAbi, TyPattern};
use hew_types::traits::MarkerTrait;

fn primitive(name: &str) -> TyPattern {
    TyPattern::Primitive(name.to_string())
}

fn hashmap_app(key: TyPattern, val: TyPattern) -> TyPattern {
    TyPattern::App {
        ctor: "HashMap".to_string(),
        args: vec![key, val],
    }
}

/// Caller-supplied predicate the resolver uses to evaluate `where`-bounds.
///
/// Mirrors the actual Hash + Eq admission policy: every scalar except
/// the float family satisfies Hash + Eq; `String` does as well (its
/// canonical hash is byte-content over the C string); `bytes` is admitted
/// here as well to exercise the bytes layout key codepath. Float remains
/// rejected — the test for `HashMap<f64, _>` is in
/// `resolved_call_registry_lookup.rs::hashmap_f64_key_rejected_*`.
fn hash_eq_satisfied(trait_name: MarkerTrait, pat: &TyPattern) -> bool {
    let name = match pat {
        TyPattern::Primitive(name) => name.as_str(),
        _ => return false,
    };
    match (trait_name, name) {
        (MarkerTrait::Hash | MarkerTrait::Eq, n) => matches!(
            n,
            "i8" | "i16"
                | "i32"
                | "i64"
                | "u8"
                | "u16"
                | "u32"
                | "u64"
                | "bool"
                | "char"
                | "String"
                | "bytes"
                | "duration"
        ),
        _ => false,
    }
}

// ---------------------------------------------------------------------------
// Scalar-key resolution coverage
// ---------------------------------------------------------------------------

/// Every admissible scalar / string / bytes K, paired with an arbitrary
/// scalar V (i64 — the choice does not matter; the resolver does not
/// distinguish V at the symbol-name level).
const ADMISSIBLE_KEYS: &[&str] = &[
    "i32", "i64", "u32", "u64", "bool", "char", "String", "bytes",
];

/// The five `Map` methods the Stage B registry exposes, paired with their
/// expected canonical kernel symbol and ABI shape.
struct ExpectedMethod {
    name: &'static str,
    symbol: &'static str,
    abi: RuntimeAbi,
}

const EXPECTED_METHODS: &[ExpectedMethod] = &[
    ExpectedMethod {
        name: "insert",
        symbol: "hew_hashmap_insert_layout",
        abi: RuntimeAbi::ByRefMut,
    },
    ExpectedMethod {
        name: "get",
        symbol: "hew_hashmap_get_layout",
        abi: RuntimeAbi::ByRef,
    },
    ExpectedMethod {
        name: "contains_key",
        symbol: "hew_hashmap_contains_key_layout",
        abi: RuntimeAbi::ByRef,
    },
    ExpectedMethod {
        name: "remove",
        symbol: "hew_hashmap_remove_layout",
        abi: RuntimeAbi::ByRefMut,
    },
    ExpectedMethod {
        name: "len",
        symbol: "hew_hashmap_len_layout",
        abi: RuntimeAbi::ByRef,
    },
];

#[test]
fn scalar_k_hashmap_resolves_to_canonical_kernel_symbols() {
    let registry = collection_dispatch_registry_for_tests();
    for key in ADMISSIBLE_KEYS {
        let receiver = hashmap_app(primitive(key), primitive("i64"));
        for expected in EXPECTED_METHODS {
            let resolved = resolve_method_call(
                &registry,
                "Map",
                expected.name,
                &receiver,
                &hash_eq_satisfied,
            )
            .unwrap_or_else(|err| {
                panic!(
                    "resolver rejected HashMap<{key}, i64>::{method} -> {err:?}",
                    method = expected.name
                )
            });

            assert_eq!(
                resolved.target.symbol_name,
                expected.symbol,
                "HashMap<{key}, i64>::{method} symbol_name drift",
                method = expected.name,
            );
            assert_eq!(
                resolved.target.abi,
                expected.abi,
                "HashMap<{key}, i64>::{method} abi drift",
                method = expected.name,
            );
            assert_eq!(
                resolved.target.call_hint,
                CallAbiHint::RuntimeShim,
                "HashMap<{key}, i64>::{method} call_hint must remain RuntimeShim through C0b",
                method = expected.name,
            );
            assert!(
                !resolved.target.consumes_receiver,
                "HashMap kernel methods do not consume the receiver (HashMap<{key}, i64>::{method})",
                method = expected.name,
            );
            assert_eq!(
                resolved.type_args,
                vec![primitive(key), primitive("i64")],
                "type_args must mirror HashMap<K, V> binding (HashMap<{key}, i64>::{method})",
                method = expected.name,
            );
        }
    }
}

// ---------------------------------------------------------------------------
// V-substitution invariance — the kernel symbol does not depend on V.
// ---------------------------------------------------------------------------

#[test]
fn hashmap_insert_symbol_is_invariant_under_value_substitution() {
    // The Stage C consumer will project (K, V) -> (key_layout, val_layout)
    // at codegen time. The *callee* name produced by the Stage B
    // resolver must be the same regardless of V — value layout selection
    // happens later, separate from method dispatch.
    let registry = collection_dispatch_registry_for_tests();
    let values: &[&str] = &[
        "i32", "i64", "u32", "u64", "f32", "f64", "bool", "char", "String", "bytes",
    ];
    for v in values {
        let receiver = hashmap_app(primitive("i64"), primitive(v));
        let resolved =
            resolve_method_call(&registry, "Map", "insert", &receiver, &hash_eq_satisfied)
                .unwrap_or_else(|err| panic!("HashMap<i64, {v}>::insert rejected: {err:?}"));
        assert_eq!(
            resolved.target.symbol_name, "hew_hashmap_insert_layout",
            "kernel symbol must be V-invariant (saw drift at V = {v})",
        );
    }
}

// ---------------------------------------------------------------------------
// HashSet parallel
// ---------------------------------------------------------------------------

#[test]
fn scalar_t_hashset_resolves_to_canonical_kernel_symbols() {
    let registry = collection_dispatch_registry_for_tests();
    let methods: &[(&str, &str, RuntimeAbi)] = &[
        ("insert", "hew_hashset_insert_layout", RuntimeAbi::ByRefMut),
        ("contains", "hew_hashset_contains_layout", RuntimeAbi::ByRef),
        ("remove", "hew_hashset_remove_layout", RuntimeAbi::ByRefMut),
        ("len", "hew_hashset_len_layout", RuntimeAbi::ByRef),
    ];
    for t in ADMISSIBLE_KEYS {
        let receiver = TyPattern::App {
            ctor: "HashSet".to_string(),
            args: vec![primitive(t)],
        };
        for (method, symbol, abi) in methods {
            let resolved =
                resolve_method_call(&registry, "Set", method, &receiver, &hash_eq_satisfied)
                    .unwrap_or_else(|err| panic!("HashSet<{t}>::{method} rejected: {err:?}"));
            assert_eq!(
                resolved.target.symbol_name, *symbol,
                "HashSet<{t}>::{method} symbol_name drift",
            );
            assert_eq!(resolved.target.abi, *abi);
            assert!(!resolved.target.consumes_receiver);
        }
    }
}
