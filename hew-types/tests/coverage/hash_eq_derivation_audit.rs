//! Type-level `MarkerTrait::Hash` and `MarkerTrait::Eq` derivation audit.
//!
//! `TypeFactService` is the authority that selects an executable Hash or Eq
//! operation for a concrete type. This test pins the broader marker substrate
//! used while resolving generic bounds. A positive marker remains necessary;
//! concrete selection may still fail for an unresolved or unsupported shape.
//!
//! In particular:
//!
//! - **Floats ARE Hash + Eq (bitwise/total).** `f32`/`f64` implement Hash and
//!   Eq via their bit pattern (NaN==NaN when bits match, reflexive; +0.0 != -0.0).
//!   Ord stays IEEE-partial.
//! - **Owned structural values remain Hash + Eq at the type level.** String,
//!   bytes, tuples and vectors rely on their selected component operations.

use hew_types::traits::{MarkerTrait, TraitRegistry};
use hew_types::ty::Ty;

fn r() -> TraitRegistry {
    TraitRegistry::new()
}

fn assert_hash_eq_pos(reg: &TraitRegistry, ty: &Ty, label: &str) {
    assert!(
        reg.implements_marker(ty, MarkerTrait::Hash),
        "{label}: expected `implements_marker(_, Hash)` = true for {ty:?}"
    );
    assert!(
        reg.implements_marker(ty, MarkerTrait::Eq),
        "{label}: expected `implements_marker(_, Eq)` = true for {ty:?}"
    );
}

fn assert_hash_eq_neg(reg: &TraitRegistry, ty: &Ty, label: &str) {
    assert!(
        !reg.implements_marker(ty, MarkerTrait::Hash),
        "{label}: expected `implements_marker(_, Hash)` = false for {ty:?}"
    );
    assert!(
        !reg.implements_marker(ty, MarkerTrait::Eq),
        "{label}: expected `implements_marker(_, Eq)` = false for {ty:?}"
    );
}

// ---- Layout-eligible primitives MUST be Hash + Eq at the type level ----

#[test]
fn fixed_width_signed_integers_are_hash_and_eq() {
    let reg = r();
    for ty in [Ty::I8, Ty::I16, Ty::I32, Ty::I64] {
        assert_hash_eq_pos(&reg, &ty, "fixed-width signed integer");
    }
}

#[test]
fn fixed_width_unsigned_integers_are_hash_and_eq() {
    let reg = r();
    for ty in [Ty::U8, Ty::U16, Ty::U32, Ty::U64] {
        assert_hash_eq_pos(&reg, &ty, "fixed-width unsigned integer");
    }
}

#[test]
fn bool_char_duration_are_hash_and_eq() {
    let reg = r();
    assert_hash_eq_pos(&reg, &Ty::Bool, "bool");
    assert_hash_eq_pos(&reg, &Ty::Char, "char");
    assert_hash_eq_pos(&reg, &Ty::Duration, "duration");
}

// ---- Floats ARE Hash + Eq under bitwise/total semantics ----

#[test]
fn floats_are_hash_and_eq_bitwise() {
    let reg = r();
    assert_hash_eq_pos(&reg, &Ty::F32, "f32 (bitwise/total)");
    assert_hash_eq_pos(&reg, &Ty::F64, "f64 (bitwise/total)");
    // Ord stays IEEE-partial: floats are PartialOrd but not Ord.
    assert!(reg.implements_marker(&Ty::F64, MarkerTrait::PartialOrd));
    assert!(!reg.implements_marker(&Ty::F64, MarkerTrait::Ord));
}

// ---- Heap-backed structural values ----

#[test]
fn string_is_hash_and_eq_at_type_level() {
    let reg = r();
    assert_hash_eq_pos(&reg, &Ty::String, "String (heap, but type-level Hash+Eq)");
}

#[test]
fn bytes_is_hash_and_eq_at_type_level() {
    let reg = r();
    assert_hash_eq_pos(&reg, &Ty::Bytes, "Bytes (heap, but type-level Hash+Eq)");
}

// ---- Composite layout-eligibility derivations ----

#[test]
fn record_of_hash_eligible_primitives_is_hash_and_eq() {
    let mut reg = r();
    reg.register_type(
        hew_types::Ty::user_for_test("Point", vec![])
            .head()
            .unwrap(),
        vec![Ty::I64, Ty::I64],
    );
    let point = Ty::named_for_test("Point", vec![]);
    assert_hash_eq_pos(&reg, &point, "record<i64, i64>");
}

#[test]
fn record_containing_float_is_hash_and_eq() {
    let mut reg = r();
    reg.register_type(
        hew_types::Ty::user_for_test("FPoint", vec![])
            .head()
            .unwrap(),
        vec![Ty::F64, Ty::F64],
    );
    let fpoint = Ty::named_for_test("FPoint", vec![]);
    // Float fields compare and hash bitwise, so the record is Hash + Eq.
    assert_hash_eq_pos(&reg, &fpoint, "record<f64, f64>");
}

// ---- Structural tuples ----

#[test]
fn tuple_of_hash_eligible_components_is_hash_and_eq_at_type_level() {
    let reg = r();
    let tup = Ty::Tuple(vec![Ty::I64, Ty::Bool]);
    assert_hash_eq_pos(&reg, &tup, "tuple<i64, bool>");
}

#[test]
fn tuple_containing_float_is_hash_and_eq() {
    let reg = r();
    let tup = Ty::Tuple(vec![Ty::I64, Ty::F32]);
    // A tuple of Hash+Eq components (floats included) is Hash + Eq.
    assert_hash_eq_pos(&reg, &tup, "tuple<i64, f32>");
}

// ---- Unresolved / platform-dependent / non-record reject paths ----
//
// At the type level: `implements_marker` accepts Isize/Usize/IntLiteral
// and FloatLiteral as Hash+Eq (they're primitives in the type system;
// floats use bitwise/total Hash+Eq). It rejects `Ty::Var` (a free var is
// not bound to any marker). The asymmetries below are documented; the test
// pins them so the substrate cannot drift.

#[test]
fn ty_var_is_not_hash_or_eq() {
    let reg = r();
    let v = Ty::Var(hew_types::ty::TypeVar(0));
    assert_hash_eq_neg(&reg, &v, "Ty::Var (unresolved)");
}

#[test]
fn float_literal_is_hash_and_eq() {
    let reg = r();
    // Literal forms inherit their numeric family's marker constraints.
    // FloatLiteral shares the F32/F64 bitwise/total Hash+Eq derivation.
    assert_hash_eq_pos(&reg, &Ty::FloatLiteral, "FloatLiteral");
}

#[test]
fn int_literal_is_hash_and_eq() {
    // IntLiteral is structurally an integer family; it implements Hash+Eq
    // at the type level. Stage C admission requires both type-level
    // marker membership AND resolution to a concrete fixed-width int;
    // the latter is enforced separately by the post-inference contract.
    let reg = r();
    assert_hash_eq_pos(&reg, &Ty::IntLiteral, "IntLiteral");
}

#[test]
fn isize_usize_are_hash_and_eq_at_type_level() {
    // Platform-dependent integers ARE Hash+Eq at the type level
    // (`implements_marker` treats them as integer primitives). The
    // Concrete capability selection separately requires a resolved supported
    // representation.
    let reg = r();
    assert_hash_eq_pos(&reg, &Ty::Isize, "isize");
    assert_hash_eq_pos(&reg, &Ty::Usize, "usize");
}
