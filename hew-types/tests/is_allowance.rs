//! Identity-comparison (`is`) allowance set and value-type rejection (slice D-2).
//!
//! Covers the checker rule from plan §D-D2:
//!
//! * Allowed: machines, actors/actor refs, `Vec`/`HashMap`/`HashSet`, `bytes`,
//!   user `type Foo { ... }` declarations.
//! * Rejected with `E_IS_VALUE_TYPE`: scalars (`int`, `bool`, `char`, floats),
//!   `String`, `record` instances, tuples.
//! * Cross-type mismatches collapse into `TypeErrorKind::Mismatch`.
//! * Move/consumed-self follows the existing use-after-move rule (plan §D-D4).
//!
//! Result type is always `bool`.

mod common;

use common::typecheck_isolated;
use hew_types::error::TypeErrorKind;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn assert_clean(src: &str) {
    let output = typecheck_isolated(src);
    assert!(
        output.errors.is_empty(),
        "expected clean type-check, got: {:#?}",
        output.errors
    );
}

fn assert_has_e_is_value_type(src: &str) {
    let output = typecheck_isolated(src);
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("E_IS_VALUE_TYPE")),
        "expected at least one `E_IS_VALUE_TYPE` error, got: {:#?}",
        output.errors
    );
}

fn assert_has_mismatch(src: &str) {
    let output = typecheck_isolated(src);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::Mismatch { .. })),
        "expected at least one `Mismatch` error, got: {:#?}",
        output.errors
    );
}

// ---------------------------------------------------------------------------
// ALLOWED: actor refs, collections, bytes, user `type Foo {...}` decls
// ---------------------------------------------------------------------------

#[test]
fn actor_ref_is_actor_ref_accepted() {
    assert_clean(
        r"
            actor Worker {
                let _id: int;
                receive fn ping() {}
            }

            fn main() {
                let a = spawn Worker(_id: 1);
                let b = spawn Worker(_id: 2);
                let _eq: bool = a is b;
            }
        ",
    );
}

#[test]
fn vec_is_vec_accepted() {
    assert_clean(
        r"
            fn main() {
                let v1: Vec<int> = Vec::new();
                let v2: Vec<int> = Vec::new();
                let _eq: bool = v1 is v2;
            }
        ",
    );
}

#[test]
fn hashmap_is_hashmap_accepted() {
    assert_clean(
        r"
            fn main() {
                let m1: HashMap<String, int> = HashMap::new();
                let m2: HashMap<String, int> = HashMap::new();
                let _eq: bool = m1 is m2;
            }
        ",
    );
}

#[test]
fn hashset_is_hashset_accepted() {
    assert_clean(
        r"
            fn main() {
                let s1: HashSet<int> = HashSet::new();
                let s2: HashSet<int> = HashSet::new();
                let _eq: bool = s1 is s2;
            }
        ",
    );
}

#[test]
fn bytes_is_bytes_accepted() {
    assert_clean(
        r"
            fn main() {
                let a = bytes::new();
                let b = bytes::new();
                let _eq: bool = a is b;
            }
        ",
    );
}

#[test]
fn user_type_decl_is_user_type_decl_accepted() {
    // `type Foo { ... }` declares a heap-backed Struct in the runtime; `is`
    // is valid (plan §D-D2).
    assert_clean(
        r"
            type Holder {
                v: int;
            }

            fn main() {
                let p = Holder { v: 1 };
                let q = Holder { v: 1 };
                let _eq: bool = p is q;
            }
        ",
    );
}

// ---------------------------------------------------------------------------
// `is` returns `bool`
// ---------------------------------------------------------------------------

#[test]
fn is_result_typed_as_bool() {
    // The `if` enforces the condition is `bool`; if `is` returned anything
    // else the `if` would fail to check.
    assert_clean(
        r"
            fn main() {
                let v1: Vec<int> = Vec::new();
                let v2: Vec<int> = Vec::new();
                if v1 is v2 {
                    let _x: int = 1;
                }
            }
        ",
    );
}

// ---------------------------------------------------------------------------
// REJECTED: scalars, String, record, tuples (E_IS_VALUE_TYPE)
// ---------------------------------------------------------------------------

#[test]
fn int_is_int_rejected() {
    assert_has_e_is_value_type(
        r"
            fn main() {
                let a: int = 1;
                let b: int = 1;
                let _eq: bool = a is b;
            }
        ",
    );
}

#[test]
fn bool_is_bool_rejected() {
    assert_has_e_is_value_type(
        r"
            fn main() {
                let a: bool = true;
                let b: bool = false;
                let _eq: bool = a is b;
            }
        ",
    );
}

#[test]
fn string_is_string_rejected() {
    assert_has_e_is_value_type(
        r#"
            fn main() {
                let a: String = "x";
                let b: String = "x";
                let _eq: bool = a is b;
            }
        "#,
    );
}

#[test]
fn record_is_record_rejected() {
    assert_has_e_is_value_type(
        r"
            record Point { x: int, y: int }

            fn main() {
                let p = Point { x: 1, y: 2 };
                let q = Point { x: 1, y: 2 };
                let _eq: bool = p is q;
            }
        ",
    );
}

#[test]
fn tuple_is_tuple_rejected() {
    assert_has_e_is_value_type(
        r"
            fn main() {
                let a: (int, int) = (1, 2);
                let b: (int, int) = (1, 2);
                let _eq: bool = a is b;
            }
        ",
    );
}

// ---------------------------------------------------------------------------
// Cross-class / cross-instantiation mismatch
// ---------------------------------------------------------------------------

#[test]
fn actor_ref_is_vec_rejected_as_mismatch() {
    // Both sides are identity-bearing, but the types differ.
    assert_has_mismatch(
        r"
            actor Worker {
                let _id: int;
                receive fn ping() {}
            }

            fn main() {
                let a = spawn Worker(_id: 1);
                let v: Vec<int> = Vec::new();
                let _eq: bool = a is v;
            }
        ",
    );
}

#[test]
fn vec_int_is_vec_string_rejected_as_mismatch() {
    // Same identity class, distinct generic instantiations.
    assert_has_mismatch(
        r"
            fn main() {
                let a: Vec<int> = Vec::new();
                let b: Vec<String> = Vec::new();
                let _eq: bool = a is b;
            }
        ",
    );
}

// ---------------------------------------------------------------------------
// Use-after-move (plan §D-D4): existing rule fires; `is` adds no special case.
// ---------------------------------------------------------------------------

#[test]
fn is_after_actor_send_emits_use_after_move() {
    // Plan §D-D4: `is` adds no special case for moved/consumed operands —
    // the existing use-after-move rule fires. Actor-send is a move site for
    // non-Copy payloads. After `s.consume(h)` moves `h`, evaluating `h is q`
    // re-uses the moved binding and must trip `UseAfterMove`.
    let src = r#"
        type Payload { data: String; }

        actor Sink {
            let _id: int;
            receive fn consume(p: Payload) {}
        }

        fn main() {
            let s = spawn Sink(_id: 0);
            let h = Payload { data: "hello" };
            let q = Payload { data: "world" };
            s.consume(h);
            let _eq: bool = h is q;
        }
    "#;
    let output = typecheck_isolated(src);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::UseAfterMove)),
        "expected `UseAfterMove` diagnostic from the existing move-checker rule, \
         got: {:#?}",
        output.errors
    );
}
