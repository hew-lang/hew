//! Identity-comparison (`is`) allowance set and value-type rejection (slice D-2).
//!
//! Covers the checker rule from plan §D-D2:
//!
//! * Allowed: machines, actors/actor refs, `Vec`/`HashMap`/`HashSet`, `bytes`,
//!   user `type Foo { ... }` declarations.
//! * Rejected with `E_IS_VALUE_TYPE`: scalars (`i64`, `bool`, `char`, floats),
//!   `string`, `record` instances, tuples.
//! * Cross-type mismatches collapse into `TypeErrorKind::Mismatch`.
//! * Move/consumed-self follows the existing use-after-move rule (plan §D-D4).
//!
//! Result type is always `bool`.

use crate::common;

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
                let _id: i64;
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
                let v1: Vec<i64> = Vec::new();
                let v2: Vec<i64> = Vec::new();
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
                let m1: HashMap<string, i64> = HashMap::new();
                let m2: HashMap<string, i64> = HashMap::new();
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
                let s1: HashSet<i64> = HashSet::new();
                let s2: HashSet<i64> = HashSet::new();
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
                v: i64;
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
                let v1: Vec<i64> = Vec::new();
                let v2: Vec<i64> = Vec::new();
                if v1 is v2 {
                    let _x: i64 = 1;
                }
            }
        ",
    );
}

// ---------------------------------------------------------------------------
// REJECTED: scalars, string, record, tuples (E_IS_VALUE_TYPE)
// ---------------------------------------------------------------------------

#[test]
fn int_is_int_rejected() {
    assert_has_e_is_value_type(
        r"
            fn main() {
                let a: i64 = 1;
                let b: i64 = 1;
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
                let a: string = "x";
                let b: string = "x";
                let _eq: bool = a is b;
            }
        "#,
    );
}

#[test]
fn record_is_record_rejected() {
    assert_has_e_is_value_type(
        r"
            record Point { x: i64, y: i64 }

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
                let a: (i64, i64) = (1, 2);
                let b: (i64, i64) = (1, 2);
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
                let _id: i64;
                receive fn ping() {}
            }

            fn main() {
                let a = spawn Worker(_id: 1);
                let v: Vec<i64> = Vec::new();
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
                let a: Vec<i64> = Vec::new();
                let b: Vec<string> = Vec::new();
                let _eq: bool = a is b;
            }
        ",
    );
}

// ---------------------------------------------------------------------------
// Actor-send snapshots do not move ordinary values.
// ---------------------------------------------------------------------------

#[test]
fn is_after_actor_send_reads_sender_snapshot_source() {
    let src = r#"
        type Payload { data: string; }

        actor Sink {
            let _id: i64;
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
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
}

// ---------------------------------------------------------------------------
// `is TypeName` (type-pattern form)
// ---------------------------------------------------------------------------

#[test]
fn is_type_pattern_static_tautology_emits_redundant_is_warning() {
    // Static-tautology: `holder is Holder` where `holder: Holder` is a
    // user `type` declaration. The checker records the type-pattern in
    // `is_type_patterns`, HIR lowers it to `HirLiteral::Bool(true)`, and
    // any `else` branch gated on the negation is dead. A `RedundantIs`
    // warning surfaces this so the user is told before they wonder why
    // their else-branch never runs.
    let output = common::typecheck_isolated(
        r"
            type Holder {
                v: i64;
            }

            fn main() {
                let holder = Holder { v: 1 };
                let _eq: bool = holder is Holder;
            }
        ",
    );
    let redundant = common::warnings_of_kind(&output, &TypeErrorKind::RedundantIs);
    assert!(
        !redundant.is_empty(),
        "expected at least one RedundantIs warning, got: {:#?}",
        output.warnings,
    );
}

#[test]
fn is_type_pattern_with_distinct_types_emits_no_redundant_is_warning() {
    // Positive-control: when the LHS type does NOT equal the RHS type
    // pattern the comparison is genuinely non-trivial — the checker should
    // flag the Mismatch (and not the static-tautology warning).
    let output = common::typecheck_isolated(
        r"
            type Holder {
                v: i64;
            }

            type Other {
                w: i64;
            }

            fn main() {
                let holder = Holder { v: 1 };
                let _eq: bool = holder is Other;
            }
        ",
    );
    let redundant = common::warnings_of_kind(&output, &TypeErrorKind::RedundantIs);
    assert!(
        redundant.is_empty(),
        "expected no RedundantIs warning when types differ, got: {:#?}",
        output.warnings,
    );
}

#[test]
fn is_type_pattern_value_type_lhs_emits_e_is_value_type() {
    // Regression coverage for the type-pattern path of E_IS_VALUE_TYPE:
    // `a is i64` where `a: i64` must reject the LHS as a value type. The
    // identity-allowance rule is the same in the type-pattern branch as
    // in the value-pattern branch; without this test the type-pattern
    // path could regress to silently admitting scalar receivers.
    assert_has_e_is_value_type(
        r"
            fn main() {
                let a: i64 = 1;
                let _eq: bool = a is i64;
            }
        ",
    );
}

#[test]
fn is_type_pattern_requires_identifier_lhs_emits_invalid_operation() {
    // Guard test for the "type patterns currently require an identifier
    // operand" `InvalidOperation` rejection: a non-Identifier LHS that
    // nonetheless produces an identity-bearing type (here a function-call
    // result) must trip the guard rather than slip through to the
    // type-pattern recording path. Using a function-call result keeps
    // the LHS identity-capable so the value-type rejection (E_IS_VALUE_TYPE)
    // doesn't fire first.
    let output = common::typecheck_isolated(
        r"
            type Holder {
                v: i64;
            }

            fn make() -> Holder {
                Holder { v: 1 }
            }

            fn main() {
                let _eq: bool = make() is Holder;
            }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::InvalidOperation)
                && e.message
                    .contains("type patterns currently require an identifier operand")),
        "expected InvalidOperation rejecting non-identifier LHS, got: {:#?}",
        output.errors,
    );
}
