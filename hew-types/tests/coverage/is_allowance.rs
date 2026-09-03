//! Identity-comparison (`is`) allowance set and value-type rejection (slice D-2).
//!
//! Covers the checker rule (HEW-SPEC-2026 §operator precedence, entry 10):
//! `is` is reference identity on heap *handles*, never on values.
//!
//! * Allowed: actors/actor refs, `Vec`/`HashMap`/`HashSet`, `bytes`.
//! * Rejected with `E_IS_VALUE_TYPE`: scalars (`i64`, `bool`, `char`, floats),
//!   `string`, tuples, user `type Foo { ... }` record declarations, `enum`
//!   declarations (`indirect` included), and machines.
//!   Records are copy-on-write values under the v0.5 value model
//!   (`docs/v05/ownership.md` — structural `==`, no pointer identity); enums
//!   and machines are tagged values, and `indirect` is a layout annotation
//!   (HEW-SPEC-2026 §3.7.4) whose heap box `is` must not expose (#3134). The
//!   checker is the last word on all of them; the codegen-front
//!   `IdentityCompare` legality check is an unreachable backstop, not a user
//!   diagnostic (#3108, #3134).
//! * Cross-type mismatches collapse into `TypeErrorKind::Mismatch`.
//! * An operand whose type is still an inference variable (a closure
//!   parameter, settled only at the call site) is decided after unification
//!   rather than abandoned, so the allowance set has no inference-shaped hole.
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
                let v1: Vec<i64> = Vec.new();
                let v2: Vec<i64> = Vec.new();
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
                let m1: HashMap<string, i64> = HashMap.new();
                let m2: HashMap<string, i64> = HashMap.new();
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
                let s1: HashSet<i64> = HashSet.new();
                let s2: HashSet<i64> = HashSet.new();
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
                let a = bytes.new();
                let b = bytes.new();
                let _eq: bool = a is b;
            }
        ",
    );
}

// ---------------------------------------------------------------------------
// REJECTED: enums and machines are values, not handles (#3134)
// ---------------------------------------------------------------------------

/// A fieldless `enum` value is a bare tag. Before #3134 the checker admitted
/// `a is b` on it and the program died in the codegen front with a span-less
/// `E_CODEGEN_FRONT_FAIL_CLOSED`.
#[test]
fn enum_is_enum_rejected() {
    assert_has_e_is_value_type(
        r"
            enum Colour {
                Red;
                Green;
            }

            fn main() {
                let a = Colour.Red;
                let b = Colour.Green;
                let _eq: bool = a is b;
            }
        ",
    );
}

/// A payload enum is still a tagged value — carrying fields does not give it
/// an address to compare.
#[test]
fn payload_enum_is_payload_enum_rejected() {
    assert_has_e_is_value_type(
        r"
            enum Shape {
                Circle(f64);
                Square(f64);
            }

            fn main() {
                let a = Shape.Circle(1.0);
                let b = Shape.Square(2.0);
                let _eq: bool = a is b;
            }
        ",
    );
}

/// The carve-out that is deliberately absent: an `indirect` enum really does
/// carry a heap box, so `is` on it used to compile and answer from the box's
/// address. `indirect` is a layout annotation (HEW-SPEC-2026 §3.7.4) and
/// admitting it here would promote it to a semantic one, making identity
/// depend on how a variant happens to be laid out. Every enum is rejected
/// alike (#3134).
#[test]
fn indirect_enum_is_indirect_enum_rejected() {
    assert_has_e_is_value_type(
        r"
            indirect enum Expr {
                Lit(i64);
                Neg(Expr);
            }

            fn main() {
                let a = Expr.Lit(1);
                let b = Expr.Lit(2);
                let _eq: bool = a is b;
            }
        ",
    );
}

/// A machine value is a tagged state with payload fields — the same value
/// class as an enum, and the same rejection.
#[test]
fn machine_is_machine_rejected() {
    assert_has_e_is_value_type(
        r"
            machine Tank {
                events {
                    Fill;
                }

                state Filling;
                state Draining;

                on Fill: Filling => Draining {
                    Tank.Draining
                }

                default { state }
            }

            fn main() {
                let t = Filling;
                let u = Draining;
                let _eq: bool = t is u;
            }
        ",
    );
}

/// The diagnostic names the enum and points at `==`, the same shape the
/// record answer takes.
#[test]
fn enum_rejection_names_the_type_and_suggests_equality() {
    let output = typecheck_isolated(
        r"
            enum Colour {
                Red;
                Green;
            }

            fn main() {
                let a = Colour.Red;
                let b = Colour.Green;
                let _eq: bool = a is b;
            }
        ",
    );
    let named = output
        .errors
        .iter()
        .filter(|e| e.message.contains("E_IS_VALUE_TYPE"))
        .filter(|e| e.message.contains("Colour") && e.message.contains("`==`"))
        .count();
    assert!(
        named > 0,
        "expected an E_IS_VALUE_TYPE naming `Colour` and `==`, got: {:#?}",
        output.errors
    );
}

// ---------------------------------------------------------------------------
// REJECTED: user `type Foo { ... }` records are values, not handles (#3108)
// ---------------------------------------------------------------------------

/// A `type Foo { ... }` declaration is a copy-on-write *value* under the v0.5
/// value model, so `p is q` has no identity to compare. Before #3108 the
/// checker admitted it and the program died in the codegen front with a
/// span-less `E_CODEGEN_FRONT_FAIL_CLOSED`; the checker must be the last word.
#[test]
fn record_type_is_record_type_rejected() {
    assert_has_e_is_value_type(
        r"
            type Point {
                x: i64;
            }

            fn main() {
                let p = Point { x: 1 };
                let q = Point { x: 1 };
                let _eq: bool = p is q;
            }
        ",
    );
}

/// One mistake, one diagnostic: `p is q` on two `Point` values is a single
/// misuse of the operator, so the two operands must not each raise a
/// byte-identical rejection.
#[test]
fn record_type_rejection_is_reported_once_per_expression() {
    let output = typecheck_isolated(
        r"
            type Point {
                x: i64;
            }

            fn main() {
                let p = Point { x: 1 };
                let q = Point { x: 1 };
                let _eq: bool = p is q;
            }
        ",
    );
    let count = output
        .errors
        .iter()
        .filter(|e| e.message.contains("E_IS_VALUE_TYPE"))
        .count();
    assert_eq!(
        count, 1,
        "expected exactly one E_IS_VALUE_TYPE, got: {:#?}",
        output.errors
    );
}

/// Counter-case for the de-duplication above: two *different* value types each
/// name their own type, so each still earns its own diagnostic. Without this
/// the de-duplication could silently collapse to "report the LHS only".
#[test]
fn distinct_value_type_operands_are_each_reported() {
    let output = typecheck_isolated(
        r#"
            fn main() {
                let a: i64 = 1;
                let b: string = "x";
                let _eq: bool = a is b;
            }
        "#,
    );
    let count = output
        .errors
        .iter()
        .filter(|e| e.message.contains("E_IS_VALUE_TYPE"))
        .count();
    assert_eq!(
        count, 2,
        "expected one E_IS_VALUE_TYPE per distinct value type, got: {:#?}",
        output.errors
    );
}

/// The diagnostic has to be actionable: it names the offending type and points
/// at `==`, the operator that does compare two records.
#[test]
fn record_type_rejection_names_the_type_and_suggests_equality() {
    let output = typecheck_isolated(
        r"
            type Point {
                x: i64;
            }

            fn main() {
                let p = Point { x: 1 };
                let q = Point { x: 1 };
                let _eq: bool = p is q;
            }
        ",
    );
    let named = output
        .errors
        .iter()
        .filter(|e| e.message.contains("E_IS_VALUE_TYPE"))
        .filter(|e| e.message.contains("Point") && e.message.contains("`==`"))
        .count();
    assert!(
        named > 0,
        "expected an E_IS_VALUE_TYPE naming `Point` and `==`, got: {:#?}",
        output.errors
    );
}

/// Negative control for the rejection: `==` on the very same record is the
/// supported comparison and must stay clean, so the diagnostic's advice is
/// truthful rather than a dead end.
#[test]
fn record_type_structural_equality_still_accepted() {
    assert_clean(
        r"
            type Point {
                x: i64;
            }

            fn main() {
                let p = Point { x: 1 };
                let q = Point { x: 1 };
                let _eq: bool = p == q;
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
                let v1: Vec<i64> = Vec.new();
                let v2: Vec<i64> = Vec.new();
                if v1 is v2 {
                    let _x: i64 = 1;
                }
            }
        ",
    );
}

// ---------------------------------------------------------------------------
// REJECTED: scalars, string, tuples (E_IS_VALUE_TYPE)
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
                let v: Vec<i64> = Vec.new();
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
                let a: Vec<i64> = Vec.new();
                let b: Vec<string> = Vec.new();
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
    let src = r"
        actor SnapshotSink {
            let _id: i64;
            receive fn consume(p: bytes) {}
        }

        fn main() {
            let s = spawn SnapshotSink(_id: 0);
            let h = bytes.new();
            let q = bytes.new();
            s.consume(h);
            let _eq: bool = h is q;
        }
    ";
    let output = typecheck_isolated(src);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
}

// ---------------------------------------------------------------------------
// `is TypeName` (type-pattern form)
// ---------------------------------------------------------------------------

#[test]
fn is_type_pattern_static_tautology_emits_redundant_is_warning() {
    // Static-tautology: `buf is bytes` where `buf: bytes`. The checker
    // records the type-pattern in `is_type_patterns`, HIR lowers it to
    // `HirLiteral::Bool(true)`, and any `else` branch gated on the negation
    // is dead. A `RedundantIs` warning surfaces this so the user is told
    // before they wonder why their else-branch never runs.
    //
    // The receiver is a `bytes` handle because the type-pattern branch runs
    // the same identity-allowance predicate as the value branch: an enum or
    // machine receiver is rejected outright (#3134).
    let output = common::typecheck_isolated(
        r"
            fn main() {
                let buf = bytes.new();
                let _eq: bool = buf is bytes;
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
            fn main() {
                let v: Vec<i64> = Vec.new();
                let _eq: bool = v is bytes;
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
            fn make() -> bytes {
                bytes.new()
            }

            fn main() {
                let _eq: bool = make() is bytes;
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

// ---------------------------------------------------------------------------
// INFERRED OPERANDS: the decision survives a closure whose parameter types
// only settle at the call site
// ---------------------------------------------------------------------------

#[test]
fn is_on_an_enum_through_an_inferred_closure_is_rejected() {
    // A closure's parameter types are inference variables while its body is
    // checked, so the `is` cannot be decided in place. Abandoning it there
    // let the program type-check and die in the codegen front on the
    // span-less `IdentityCompare lhs must be a pointer or integer value`
    // (#3134); the obligation is re-run after unification instead.
    assert_has_e_is_value_type(
        r"
            enum Colour {
                Red;
                Green;
            }

            fn main() {
                let same = |a, b| a is b;
                let _eq: bool = same(Colour.Red, Colour.Green);
            }
        ",
    );
}

#[test]
fn is_on_a_record_through_an_inferred_closure_is_rejected() {
    assert_has_e_is_value_type(
        r"
            type Point {
                x: i64;
            }

            fn main() {
                let same = |a, b| a is b;
                let _eq: bool = same(Point { x: 1 }, Point { x: 2 });
            }
        ",
    );
}

#[test]
fn is_on_bytes_through_an_inferred_closure_is_accepted() {
    // Negative control: an inferred operand is not itself the fault. Without
    // this, rejecting every unresolved operand would also pass the two tests
    // above.
    assert_clean(
        r"
            fn main() {
                let same = |a, b| a is b;
                let x = bytes.new();
                let y = bytes.new();
                let _eq: bool = same(x, y);
            }
        ",
    );
}

#[test]
fn mismatched_handle_types_through_an_inferred_closure_are_reported() {
    // The mismatch half of the deferred decision: both operands are
    // identity-capable, so the rejection is the cross-type `Mismatch` rather
    // than `E_IS_VALUE_TYPE`, and it must survive the deferral too.
    assert_has_mismatch(
        r"
            fn main() {
                let same = |a, b| a is b;
                let v: Vec<i64> = Vec.new();
                let b = bytes.new();
                let _eq: bool = same(v, b);
            }
        ",
    );
}
