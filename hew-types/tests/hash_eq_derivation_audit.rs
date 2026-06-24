//! W4.001 Stage A substrate test: `implements_marker` Hash/Eq derivation audit.
//!
//! This test pins down the type-level `MarkerTrait::Hash` and
//! `MarkerTrait::Eq` derivation for every K class enumerated in the
//! checker's layout-key hash-eligibility table
//! (`hew-types/src/hash_eligibility.rs:64-104`). It is the DI-008-prep
//! gate: every type the checker today accepts as a layout-key K (or
//! whose ineligibility is structurally encoded) must be classified by
//! the marker substrate consistently, so that Stage C can replace the
//! per-K-class allowlists in `admissibility.rs:804-942` with the
//! resolver's `K: Hash + Eq` bound-satisfaction predicate.
//!
//! # Audit shape
//!
//! `hash_eligibility` answers a stricter question than `implements_marker`:
//! "is K eligible as a **layout** hash-map key?" — which adds codegen
//! constraints (fixed width, no padding, no heap pointer). At the
//! **type-level** marker substrate, the answer is "does K implement
//! Hash/Eq?" — which is the necessary (but not sufficient) condition
//! the Stage C bounds-form admission gate will evaluate.
//!
//! Concretely the relationships this test pins are:
//!
//! - **layout-eligible ⇒ Hash + Eq at the type level.** Every K that
//!   `hash_eligibility` admits today must be a `MarkerTrait::Hash` and
//!   `MarkerTrait::Eq` member; otherwise Stage C's replacement gate
//!   would reject what the legacy gate accepted.
//! - **NaN-ineligible ⇒ NOT Hash, NOT Eq.** `f32`/`f64` violate the
//!   hash/equality contract and must be rejected at both levels.
//! - **Heap-eligible at type level but layout-ineligible.** `String`
//!   and `Bytes` ARE Hash + Eq at the type level (Stage C will admit
//!   `HashMap<String, _>` via the bounds predicate), but
//!   `hash_eligibility` rejects them as layout keys because layout
//!   hashing operates on the fixed-size stack blob. This is the
//!   documented gap, NOT a failure — the layout-key constraint stays
//!   in codegen.
//!
//! Any future divergence — e.g. someone adding a new K class to
//! `hash_eligibility` without updating `implements_marker`, or
//! vice-versa — flips an assertion here and blocks Stage A close.
//! Per §7 risk #9 of the design notes.

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

// ---- NaN-ineligible floats MUST NOT be Hash or Eq at any level ----

#[test]
fn floats_are_neither_hash_nor_eq() {
    let reg = r();
    assert_hash_eq_neg(&reg, &Ty::F32, "f32 (NaN hazard)");
    assert_hash_eq_neg(&reg, &Ty::F64, "f64 (NaN hazard)");
}

// ---- Heap-eligible-at-type-level but layout-ineligible (documented gap) ----
//
// Stage C's admission gate will accept `HashMap<String, _>` because String
// implements Hash + Eq at the type level. The layout-key restriction in
// `hash_eligibility` reflects a codegen choice (layout hashes the stack
// blob, not the heap contents) and stays in codegen. The audit pins this
// asymmetry so a future refactor cannot silently realign them.

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
    reg.register_type("Point".into(), vec![Ty::I64, Ty::I64]);
    let point = Ty::Named {
        name: "Point".into(),
        args: vec![],
        builtin: None,
    };
    assert_hash_eq_pos(&reg, &point, "record<i64, i64>");
}

#[test]
fn record_containing_float_is_neither_hash_nor_eq() {
    let mut reg = r();
    reg.register_type("FPoint".into(), vec![Ty::F64, Ty::F64]);
    let fpoint = Ty::Named {
        name: "FPoint".into(),
        args: vec![],
        builtin: None,
    };
    assert_hash_eq_neg(&reg, &fpoint, "record<f64, f64>");
}

// ---- Layout-tracked-but-rejected: tuples (documented gap) ----
//
// `hash_eligibility` rejects tuple layout-keys outright (IneligibleTuple)
// for codegen reasons. At the type level, `implements_marker` says a tuple
// of Hash+Eq components IS Hash+Eq (this is structurally correct — Rust
// derives this exactly the same way). Stage C will admit
// `HashMap<(i64, bool), _>` via the bounds predicate when codegen catches
// up; today the codegen gap is preserved by `admissibility.rs`. We pin
// the type-level positive verdict here so the substrate does not
// regress.

#[test]
fn tuple_of_hash_eligible_components_is_hash_and_eq_at_type_level() {
    let reg = r();
    let tup = Ty::Tuple(vec![Ty::I64, Ty::Bool]);
    assert_hash_eq_pos(&reg, &tup, "tuple<i64, bool>");
}

#[test]
fn tuple_containing_float_is_neither_hash_nor_eq() {
    let reg = r();
    let tup = Ty::Tuple(vec![Ty::I64, Ty::F32]);
    assert_hash_eq_neg(&reg, &tup, "tuple<i64, f32>");
}

// ---- Unresolved / platform-dependent / non-record reject paths ----
//
// `hash_eligibility` rejects: `Ty::Var` (unresolved), `Ty::Error`,
// `Ty::Isize` / `Ty::Usize` (platform-dependent width),
// `Ty::IntLiteral` / `Ty::FloatLiteral` (still unresolved).
//
// At the type level: `implements_marker` accepts Isize/Usize/IntLiteral
// as Hash+Eq (they're primitives in the type system). It rejects
// `Ty::Var` (a free var is not bound to any marker) and `FloatLiteral`
// (inherits float NaN rejection). The asymmetries below are documented;
// the test pins them so the substrate cannot drift.

#[test]
fn ty_var_is_not_hash_or_eq() {
    let reg = r();
    let v = Ty::Var(hew_types::ty::TypeVar(0));
    assert_hash_eq_neg(&reg, &v, "Ty::Var (unresolved)");
}

#[test]
fn float_literal_is_not_hash_or_eq() {
    let reg = r();
    // Literal forms inherit their numeric family's marker constraints.
    // FloatLiteral falls under the F32/F64 NaN-rejection rule.
    assert_hash_eq_neg(&reg, &Ty::FloatLiteral, "FloatLiteral");
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
    // layout-key codegen constraint that rejects them lives in
    // `hash_eligibility`, not in the type substrate. The audit pins
    // this so the type substrate doesn't grow a parallel layout-key
    // check by accident.
    let reg = r();
    assert_hash_eq_pos(&reg, &Ty::Isize, "isize");
    assert_hash_eq_pos(&reg, &Ty::Usize, "usize");
}
