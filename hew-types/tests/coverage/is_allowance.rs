//! Identity-comparison (`is`) allowance set and value-type rejection (slice D-2, D340).
//!
//! Covers the checker rule (HEW-SPEC-2026 §3.4.3, §operator precedence entry
//! 10): `is` is handle identity only. D340 ratified §3.4.3's category table;
//! `is_identity_capable` currently implements its pid handle row (actors and
//! `LocalPid<T>`) — the counted/opaque/resource handle rows (`Rc`, `Weak`,
//! `#[opaque]`/`#[resource]` wrappers) are still refused pending the codegen
//! support their identity words need, tracked outside this PR.
//!
//! * Allowed: actors/actor refs (`LocalPid<T>`).
//! * Rejected with `E_IS_VALUE_TYPE`: scalars (`i64`, `bool`, `char`, floats),
//!   `string`, `bytes`, tuples, `Vec`/`HashMap`/`HashSet`, user
//!   `type Foo { ... }` record declarations, `enum` declarations (`indirect`
//!   included), machines, `dyn Trait` objects, and closures.
//!   Records, `bytes`, and the collections are copy-on-write values under the
//!   v0.5 value model (`docs/v05/ownership.md` — structural `==`, no pointer
//!   identity, HEW-SPEC-2026 §3.4.3's value row); enums and machines are
//!   tagged values, and `indirect` is a layout annotation (HEW-SPEC-2026
//!   §3.7.4) whose heap box `is` must not expose (#3134). The checker is the
//!   last word on all of them; the codegen-front `IdentityCompare` legality
//!   check is an unreachable backstop, not a user diagnostic (#3108, #3134).
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
// ALLOWED: actor refs (the pid handle row of HEW-SPEC-2026 §3.4.3)
// ---------------------------------------------------------------------------

#[test]
fn actor_ref_is_actor_ref_accepted() {
    assert_clean(
        r"
            actor Worker {
                let _id: i64,
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

// ---------------------------------------------------------------------------
// REJECTED: `Vec`/`HashMap`/`HashSet`/`bytes` are values, not handles (D340)
//
// D340 narrows the admission set to the pid handle row only: collections and
// `bytes` are copy-on-write values with structural `==` (HEW-SPEC-2026
// §3.4.3's value row), so `a is b` on them is `E_IS_VALUE_TYPE` exactly like
// an enum or record — the codegen support this PR's predecessor added for
// `bytes` identity (`BytesTriple` field-0 `ptrtoint`) is unreachable now and
// was deleted with it.
// ---------------------------------------------------------------------------

#[test]
fn vec_is_vec_rejected() {
    assert_has_e_is_value_type(
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
fn hashmap_is_hashmap_rejected() {
    assert_has_e_is_value_type(
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
fn hashset_is_hashset_rejected() {
    assert_has_e_is_value_type(
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
fn bytes_is_bytes_rejected() {
    assert_has_e_is_value_type(
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
// Negative controls: narrowing the admission set to reject the four value
// types above must not disturb any other operand's existing answer.
// ---------------------------------------------------------------------------

/// `LocalPid` stays admitted — the case `actor_ref_is_actor_ref_accepted`
/// above already pins; this negative control names it explicitly alongside
/// its three siblings below so the four are read together.
#[test]
fn negative_control_local_pid_still_accepted() {
    assert_clean(
        r"
            actor Worker {
                let _id: i64,
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

/// `Rc` is HEW-SPEC-2026 §3.4.3's counted-handle row, not yet wired into
/// `is_identity_capable` (its identity word needs codegen support this PR
/// does not add) — still refused as a value type, unchanged by this PR's
/// narrowing edit.
#[test]
fn negative_control_rc_still_rejected() {
    assert_has_e_is_value_type(
        r"
            fn main() {
                let a = Rc.new(1);
                let b = Rc.new(2);
                let _eq: bool = a is b;
            }
        ",
    );
}

/// `dyn Trait` is HEW-SPEC-2026 §3.4.3's value row — refused before this PR
/// and unchanged by it.
#[test]
fn negative_control_dyn_trait_still_rejected() {
    assert_has_e_is_value_type(
        r#"
            trait Greeter {
                fn greet(self) -> string;
            }

            type EnglishGreeter {
                name: string,
            }

            impl Greeter for EnglishGreeter {
                fn greet(self) -> string {
                    self.name
                }
            }

            fn main() {
                let a: dyn Greeter = EnglishGreeter { name: "a" };
                let b: dyn Greeter = EnglishGreeter { name: "b" };
                let _eq: bool = a is b;
            }
        "#,
    );
}

/// Closures are HEW-SPEC-2026 §3.4.3's callable row — refused before this PR
/// and unchanged by it.
#[test]
fn negative_control_closures_still_rejected() {
    assert_has_e_is_value_type(
        r"
            fn main() {
                let f = || true;
                let g = || true;
                let _eq: bool = f is g;
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
                Red,
                Green,
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
                Circle(f64),
                Square(f64),
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
                Lit(i64),
                Neg(Expr),
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
                    Fill,
                }

                state Filling,
                state Draining,

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
                Red,
                Green,
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
                x: i64,
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
                x: i64,
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
                x: i64,
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
                x: i64,
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
            actor Worker {
                let _id: i64,
                receive fn ping() {}
            }

            fn main() {
                let a = spawn Worker(_id: 1);
                let b = spawn Worker(_id: 2);
                if a is b {
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
// Cross-instantiation mismatch
//
// D340 narrowed the admission set to the pid handle row alone, so a
// cross-*class* mismatch (two different identity-bearing categories, e.g. an
// actor against a `Vec`) has no reachable example left — `Vec` is a value
// type now and reports `E_IS_VALUE_TYPE` on its own operand rather than
// joining a `Mismatch`. Two different actor types are still both
// identity-capable and distinct, so that cross-instantiation shape survives.
// ---------------------------------------------------------------------------

#[test]
fn actor_refs_of_different_actor_types_rejected_as_mismatch() {
    assert_has_mismatch(
        r"
            actor Worker {
                let _id: i64,
                receive fn ping() {}
            }

            actor Other {
                let _id: i64,
                receive fn ping() {}
            }

            fn main() {
                let a = spawn Worker(_id: 1);
                let b = spawn Other(_id: 1);
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
    // `bytes` is a value type (D340), so `==` is the comparison this proves:
    // the actor send snapshots `h` rather than moving it, so the sender's
    // binding is still live afterwards. `is` would reject `bytes` on its own
    // terms regardless of the move question this test is actually about.
    let src = r"
        actor SnapshotSink {
            let _id: i64,
            receive fn consume(p: bytes) {}
        }

        fn main() {
            let s = spawn SnapshotSink(_id: 0);
            let h = bytes.new();
            let q = bytes.new();
            s.consume(h);
            let _eq: bool = h == q;
        }
    ";
    let output = typecheck_isolated(src);
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
}

// ---------------------------------------------------------------------------
// `is TypeName` (type-pattern form)
//
// D340 narrows admission to actor handles, and `resolve_is_type_pattern`
// always resolves a `TypeName` RHS to the bare `TypeDef` name with no
// generic arguments — every admitted actor value is a `LocalPid<T>` handle,
// so an LHS can never structurally equal that bare pattern
// (`LocalPid<Worker>` vs. `Worker`). The static-tautology branch
// (`HirLiteral::Bool(true)`, the `RedundantIs` warning, and the
// "type patterns currently require an identifier operand" guard, which only
// fires alongside the tautology) has no reachable positive control left; see
// the module doc comment. Only the still-reachable rejection and mismatch
// paths are tested here.
// ---------------------------------------------------------------------------

#[test]
fn is_type_pattern_with_distinct_types_emits_no_redundant_is_warning() {
    // `this is Worker` inside a `Worker` receive fn: `this: LocalPid<Worker>`
    // never equals the bare `Worker` type pattern, so the checker reports the
    // Mismatch this test's name promises, not the static-tautology warning.
    let output = common::typecheck_isolated(
        r"
            actor Worker {
                let _id: i64,
                receive fn ping() -> bool {
                    this is Worker
                }
            }

            fn main() {}
        ",
    );
    let redundant = common::warnings_of_kind(&output, &TypeErrorKind::RedundantIs);
    assert!(
        redundant.is_empty(),
        "expected no RedundantIs warning when types differ, got: {:#?}",
        output.warnings,
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, TypeErrorKind::Mismatch { .. })),
        "expected a Mismatch between `LocalPid<Worker>` and `Worker`, got: {:#?}",
        output.errors,
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
                Red,
                Green,
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
                x: i64,
            }

            fn main() {
                let same = |a, b| a is b;
                let _eq: bool = same(Point { x: 1 }, Point { x: 2 });
            }
        ",
    );
}

#[test]
fn is_on_an_actor_ref_through_an_inferred_closure_is_accepted() {
    // Negative control: an inferred operand is not itself the fault. Without
    // this, rejecting every unresolved operand would also pass the two tests
    // above.
    assert_clean(
        r"
            actor Worker {
                let _id: i64,
                receive fn ping() {}
            }

            fn main() {
                let same = |a, b| a is b;
                let x = spawn Worker(_id: 1);
                let y = spawn Worker(_id: 2);
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
            actor Worker {
                let _id: i64,
                receive fn ping() {}
            }

            actor Other {
                let _id: i64,
                receive fn ping() {}
            }

            fn main() {
                let same = |a, b| a is b;
                let w = spawn Worker(_id: 1);
                let o = spawn Other(_id: 1);
                let _eq: bool = same(w, o);
            }
        ",
    );
}

/// An inferred `is` operand that never resolves at all — the closure is
/// passed to a generic sink and never called, so `a`/`b` stay `Ty::Var`
/// through the end of inference. The deferred re-check must refuse this
/// itself (fail closed) rather than silently drop the obligation and let it
/// reach the codegen front's span-less `IdentityCompare` backstop (#3134).
#[test]
fn is_on_an_unresolved_inferred_operand_fails_closed() {
    let output = common::typecheck_isolated(
        r"
            fn sink<F>(_f: F) {}

            fn main() {
                sink(|a, b| a is b);
            }
        ",
    );
    assert!(
        output.errors.iter().any(|e| e
            .message
            .contains("cannot infer type for `is` operand type")),
        "expected the `is` re-check to report its own inference-failed diagnostic \
         for a still-unresolved operand, got: {:#?}",
        output.errors
    );
}
