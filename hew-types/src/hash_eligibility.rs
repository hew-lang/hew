//! Checker-side hash eligibility for layout-backed `HashMap` keys and `HashSet` elements.
//!
//! # Contract
//!
//! A type is hash-eligible when its hash thunk can operate on typed field values
//! with **deterministic fixed widths** — never on raw struct bytes, and never on
//! padding. The field-typed hashing rules are:
//!
//! - **bool fields**: load as `i8`, hash the low bit only (mask `0x1`).
//! - **char fields**: load as `i32`, hash all 4 bytes of the Unicode scalar value.
//! - **integer fields**: hash the exact-width value load (`i8`..`i64`, `u8`..`u64`).
//! - **duration fields**: load as `i64` nanoseconds (a duration is always 8 bytes).
//!
//! Floats are **never** hash-eligible because `NaN != NaN` violates the
//! hash-equality contract: two NaN values may be `!=` yet hash to the same bucket,
//! making every `HashMap<FloatRecord, V>` lookup semantically broken.
//!
//! Strings and other heap-managed types are ineligible because layout-ABI hashing
//! operates on the fixed-size stack blob, not the heap contents. String equality
//! must go through the existing string-ABI path, not the layout path.
//!
//! Named records are eligible only when every field is itself hash-eligible.
//! Indirect (handle/managed) records are ineligible regardless of field content.
//! Tuples are tracked but ineligible as layout hash keys in this slice; the
//! admissibility gate (C-2c) will reject them before they reach codegen.

use crate::check::{TypeDef, TypeDefKind};
use crate::eligibility_walker::walk_fields_for_eligibility;
use crate::ty::Ty;
use std::collections::HashMap;

/// The hash eligibility verdict for a type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum HashEligibility {
    /// Structurally hashable on typed field values with deterministic fixed widths.
    Eligible,
    /// Contains a floating-point field (or is itself a float). `NaN != NaN`
    /// torpedoes the hash-equality contract.
    IneligibleFloat(Ty),
    /// Is or contains a heap-managed component (String, Bytes, indirect record).
    IneligibleManaged(Ty),
    /// Is or contains an owned, non-Copy component (closure, function, trait object,
    /// unknown named type, platform-sized integer, etc.).
    IneligibleOwned(Ty),
    /// Is a tuple. Tuple keys are tracked here but not admitted as layout hash keys.
    IneligibleTuple(Ty),
    /// Is a named type whose `TypeDefKind` is not `Record` (e.g. `Enum`, `Struct`,
    /// `Actor`, `Machine`). Only `record`-keyword types are eligible as layout hash
    /// keys because only they are guaranteed Copy value-semantic at the language level.
    IneligibleNamedNonRecord(Ty),
    /// Is an unresolved type-inference variable.
    IneligibleVar,
    /// Is the error-recovery type (`Ty::Error`).
    IneligibleError,
}

/// Returns `Some(rejection)` if `ty` is not hash-eligible, `None` if eligible.
///
/// This inner form is passed as a `fn` pointer to
/// [`walk_fields_for_eligibility`] so that named-type field recursion
/// is shared with [`crate::eq_eligibility`] without code duplication.
fn hash_ineligibility(ty: &Ty, type_defs: &HashMap<String, TypeDef>) -> Option<HashEligibility> {
    match ty {
        // Fixed-width integer primitives — hash the exact-width value load.
        // bool: hash the low bit as i8. char: hash 4-byte Unicode scalar as i32.
        // Duration: hash 8-byte i64 nanosecond value — deterministic fixed width.
        Ty::I8
        | Ty::I16
        | Ty::I32
        | Ty::I64
        | Ty::U8
        | Ty::U16
        | Ty::U32
        | Ty::U64
        | Ty::Bool
        | Ty::Char
        | Ty::Duration => None,

        // Floats: NaN != NaN violates the hash-equality contract.
        Ty::F32 | Ty::F64 => Some(HashEligibility::IneligibleFloat(ty.clone())),

        // String: heap-managed; hashing must go through the string ABI, not layout.
        Ty::String => Some(HashEligibility::IneligibleManaged(ty.clone())),

        // Tuples: tracked but not admitted as layout hash keys in this slice.
        Ty::Tuple(_) => Some(HashEligibility::IneligibleTuple(ty.clone())),

        // Named types: eligible iff Record kind, not indirect, and every field is
        // hash-eligible. Only `record`-keyword types (`TypeDefKind::Record`) are
        // Copy value-semantic in Hew; Struct/Enum/Actor/Machine are not layout keys.
        Ty::Named { name, .. } => match type_defs.get(name) {
            Some(type_def) if type_def.is_indirect => {
                Some(HashEligibility::IneligibleManaged(ty.clone()))
            }
            Some(type_def) if type_def.kind != TypeDefKind::Record => {
                Some(HashEligibility::IneligibleNamedNonRecord(ty.clone()))
            }
            Some(type_def) => walk_fields_for_eligibility(type_def, type_defs, hash_ineligibility),
            // Unknown named type — fail closed; cannot verify hash-eligibility.
            None => Some(HashEligibility::IneligibleOwned(ty.clone())),
        },

        // Unresolved inference variable.
        Ty::Var(_) => Some(HashEligibility::IneligibleVar),

        // Error-recovery type.
        Ty::Error => Some(HashEligibility::IneligibleError),

        // All remaining types are not fixed-width primitives or Copy plain records.
        // IntLiteral/FloatLiteral: still unresolved — cannot determine hash width.
        // Isize/Usize: platform-dependent width — violates deterministic-width contract.
        // Unit: no practical use as a hash key; excluded for conservatism.
        // Never: diverging type, cannot be a valid key.
        Ty::IntLiteral
        | Ty::FloatLiteral
        | Ty::Never
        | Ty::Unit
        | Ty::Isize
        | Ty::Usize
        | Ty::Bytes
        | Ty::Array(_, _)
        | Ty::Slice(_)
        | Ty::Function { .. }
        | Ty::Closure { .. }
        | Ty::Pointer { .. }
        | Ty::TraitObject { .. }
        | Ty::Task(_)
        | Ty::AssocType { .. } => Some(HashEligibility::IneligibleOwned(ty.clone())),
    }
}

/// Determine whether `ty` is eligible for use as a layout hash key or element.
///
/// Returns [`HashEligibility::Eligible`] when the type can be hashed via a
/// field-typed thunk with deterministic fixed-width loads (no padding, no
/// float-equality hazards, no heap pointers). Named records are eligible only
/// when they are not indirect and every field passes the same check recursively.
#[must_use]
pub(crate) fn ty_is_hash_eligible(
    ty: &Ty,
    type_defs: &HashMap<String, TypeDef>,
) -> HashEligibility {
    hash_ineligibility(ty, type_defs).unwrap_or(HashEligibility::Eligible)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::check::{FnSig, VariantDef};
    use crate::ty::TypeVar;

    fn empty_type_defs() -> HashMap<String, TypeDef> {
        HashMap::new()
    }

    /// Build a `record`-keyword type def (Copy value-semantic, eligible for field-walk).
    fn make_record(name: &str, fields: Vec<(&str, Ty)>, is_indirect: bool) -> TypeDef {
        let field_order: Vec<String> = fields.iter().map(|(n, _)| n.to_string()).collect();
        TypeDef {
            kind: TypeDefKind::Record,
            name: name.to_string(),
            type_params: vec![],
            fields: fields
                .into_iter()
                .map(|(n, t)| (n.to_string(), t))
                .collect(),
            field_order,
            variants: HashMap::<String, VariantDef>::new(),
            methods: HashMap::<String, FnSig>::new(),
            doc_comment: None,
            is_indirect,
        }
    }

    /// Build a non-record named type def (Enum, Struct, etc.) — never eligible as layout key.
    fn make_non_record(name: &str, kind: TypeDefKind) -> TypeDef {
        TypeDef {
            kind,
            name: name.to_string(),
            type_params: vec![],
            fields: HashMap::new(), // empty fields — the gap: without kind check this returns Eligible
            field_order: vec![],
            variants: HashMap::<String, VariantDef>::new(),
            methods: HashMap::<String, FnSig>::new(),
            doc_comment: None,
            is_indirect: false,
        }
    }

    #[test]
    fn hash_eligible_integers_all_widths() {
        let tds = empty_type_defs();
        for ty in [
            Ty::I8,
            Ty::I16,
            Ty::I32,
            Ty::I64,
            Ty::U8,
            Ty::U16,
            Ty::U32,
            Ty::U64,
        ] {
            assert_eq!(
                ty_is_hash_eligible(&ty, &tds),
                HashEligibility::Eligible,
                "{ty:?} should be hash-eligible"
            );
        }
    }

    #[test]
    fn hash_eligible_bool_and_char() {
        let tds = empty_type_defs();
        assert_eq!(
            ty_is_hash_eligible(&Ty::Bool, &tds),
            HashEligibility::Eligible
        );
        assert_eq!(
            ty_is_hash_eligible(&Ty::Char, &tds),
            HashEligibility::Eligible
        );
    }

    #[test]
    fn hash_eligible_duration() {
        let tds = empty_type_defs();
        // Duration is i64 nanoseconds: deterministic fixed width, no NaN hazard.
        assert_eq!(
            ty_is_hash_eligible(&Ty::Duration, &tds),
            HashEligibility::Eligible
        );
    }

    #[test]
    fn hash_ineligible_float_f64() {
        let tds = empty_type_defs();
        assert_eq!(
            ty_is_hash_eligible(&Ty::F64, &tds),
            HashEligibility::IneligibleFloat(Ty::F64)
        );
        assert_eq!(
            ty_is_hash_eligible(&Ty::F32, &tds),
            HashEligibility::IneligibleFloat(Ty::F32)
        );
    }

    #[test]
    fn hash_ineligible_string() {
        let tds = empty_type_defs();
        assert_eq!(
            ty_is_hash_eligible(&Ty::String, &tds),
            HashEligibility::IneligibleManaged(Ty::String)
        );
    }

    #[test]
    fn hash_eligible_copy_record_int_fields() {
        let mut tds = HashMap::new();
        tds.insert(
            "Point".to_string(),
            make_record("Point", vec![("x", Ty::I64), ("y", Ty::I64)], false),
        );
        let ty = Ty::normalize_named("Point".to_string(), vec![]);
        assert_eq!(ty_is_hash_eligible(&ty, &tds), HashEligibility::Eligible);
    }

    #[test]
    fn hash_ineligible_record_with_float_field() {
        let mut tds = HashMap::new();
        tds.insert(
            "FPoint".to_string(),
            make_record("FPoint", vec![("x", Ty::F64), ("y", Ty::F64)], false),
        );
        let ty = Ty::normalize_named("FPoint".to_string(), vec![]);
        assert_eq!(
            ty_is_hash_eligible(&ty, &tds),
            HashEligibility::IneligibleFloat(Ty::F64)
        );
    }

    #[test]
    fn hash_ineligible_record_with_string_field() {
        let mut tds = HashMap::new();
        tds.insert(
            "Named".to_string(),
            make_record("Named", vec![("label", Ty::String)], false),
        );
        let ty = Ty::normalize_named("Named".to_string(), vec![]);
        assert_eq!(
            ty_is_hash_eligible(&ty, &tds),
            HashEligibility::IneligibleManaged(Ty::String)
        );
    }

    #[test]
    fn hash_ineligible_managed_record() {
        let mut tds = HashMap::new();
        // is_indirect takes precedence over kind check — checked regardless of Record/Struct.
        tds.insert(
            "Handle".to_string(),
            make_record("Handle", vec![], true), // is_indirect = true
        );
        let ty = Ty::normalize_named("Handle".to_string(), vec![]);
        assert_eq!(
            ty_is_hash_eligible(&ty, &tds),
            HashEligibility::IneligibleManaged(ty.clone())
        );
    }

    #[test]
    fn hash_ineligible_tuple() {
        let tds = empty_type_defs();
        let ty = Ty::Tuple(vec![Ty::I32, Ty::I64]);
        assert_eq!(
            ty_is_hash_eligible(&ty, &tds),
            HashEligibility::IneligibleTuple(ty.clone())
        );
    }

    #[test]
    fn hash_ineligible_ty_var_and_ty_error() {
        let tds = empty_type_defs();
        assert_eq!(
            ty_is_hash_eligible(&Ty::Var(TypeVar(0)), &tds),
            HashEligibility::IneligibleVar
        );
        assert_eq!(
            ty_is_hash_eligible(&Ty::Error, &tds),
            HashEligibility::IneligibleError
        );
    }

    /// Verify that a `record` type with bool/char/duration fields is hash-eligible
    /// (exercising the non-integer primitive path through the field-walk).
    #[test]
    fn hash_eligible_copy_record_mixed_primitives() {
        let mut tds = HashMap::new();
        tds.insert(
            "Event".to_string(),
            make_record(
                "Event",
                vec![
                    ("active", Ty::Bool),
                    ("code", Ty::Char),
                    ("ts", Ty::Duration),
                    ("tag", Ty::I32),
                ],
                false,
            ),
        );
        let ty = Ty::normalize_named("Event".to_string(), vec![]);
        assert_eq!(ty_is_hash_eligible(&ty, &tds), HashEligibility::Eligible);
    }

    /// Regression: a non-`record` named type (e.g. an enum) with empty fields must NOT
    /// be eligible even though the field-walk would trivially return `None` rejections.
    /// Without the `TypeDefKind::Record` guard, an enum would slip through as `Eligible`.
    #[test]
    fn hash_ineligible_enum_named_type() {
        let mut tds = HashMap::new();
        tds.insert(
            "Color".to_string(),
            make_non_record("Color", TypeDefKind::Enum),
        );
        let ty = Ty::normalize_named("Color".to_string(), vec![]);
        assert_eq!(
            ty_is_hash_eligible(&ty, &tds),
            HashEligibility::IneligibleNamedNonRecord(ty.clone()),
            "Enum types must be rejected before field-walk even when fields is empty"
        );
    }

    /// Regression: a `Struct`-kind named type (not declared with `record`) is also
    /// rejected — Struct types are not guaranteed Copy value-semantic in Hew.
    #[test]
    fn hash_ineligible_struct_named_type() {
        let mut tds = HashMap::new();
        tds.insert(
            "Wrapper".to_string(),
            make_non_record("Wrapper", TypeDefKind::Struct),
        );
        let ty = Ty::normalize_named("Wrapper".to_string(), vec![]);
        assert_eq!(
            ty_is_hash_eligible(&ty, &tds),
            HashEligibility::IneligibleNamedNonRecord(ty.clone()),
        );
    }

    /// Isize and Usize are platform-dependent width — violates the deterministic-width
    /// contract required by the layout hash thunk.
    #[test]
    fn hash_ineligible_isize_and_usize() {
        let tds = empty_type_defs();
        assert_eq!(
            ty_is_hash_eligible(&Ty::Isize, &tds),
            HashEligibility::IneligibleOwned(Ty::Isize),
            "isize has platform-dependent width — ineligible for layout hash"
        );
        assert_eq!(
            ty_is_hash_eligible(&Ty::Usize, &tds),
            HashEligibility::IneligibleOwned(Ty::Usize),
            "usize has platform-dependent width — ineligible for layout hash"
        );
    }
}
