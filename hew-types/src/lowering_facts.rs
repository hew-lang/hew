//! Checker-owned lowering metadata for erased runtime types.
//!
//! # Lifecycle
//!
//! `HashMap` and `HashSet` layout facts are authored by the type-checker
//! admissibility gate and begin in the [`HashMapLoweringFactState::Pending`]
//! state.  Codegen consumes `Pending` facts, emits the required layout globals
//! and thunks, then transitions each fact to `Finalized`.  The
//! [`assert_lowering_facts_consistent`] output-contract check verifies that
//! every `Finalized` layout fact carries the expected size and alignment before
//! being handed to downstream consumers.

use crate::Ty;
use serde::{Deserialize, Serialize};

// ── Existing types — scalar HashSet path ──────────────────────────────────────

/// High-level lowering lane identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum LoweringKind {
    HashSet,
    /// Layout-keyed or scalar-keyed `HashMap` lowering.
    ///
    /// Wired into admissibility in C-2c.
    HashMap,
}

/// Checker-authoritative `HashSet` element kinds that codegen may consume.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum HashSetElementType {
    I64,
    U64,
    Str,
}

/// Runtime ABI selector for lowered `HashSet` operations.
///
/// The `Layout` variant is new in this slice and wired into admissibility in
/// C-2c.  `Copy` is intentionally not derived: `Layout` carries the element
/// record name as an owned string.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum HashSetAbi {
    Int64,
    String,
    /// Copy named-record element backed by the `hew_hashset_*_layout` ABI.
    ///
    /// Wired into admissibility in C-2c.
    Layout {
        /// Name of the record type used as the set element (e.g. `"Point"`).
        elem_record_name: std::string::String,
    },
}

/// Drop lane selector for lowered runtime handles.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DropKind {
    HashSetFree,
}

/// A single checker-owned lowering fact for a scalar-ABI `HashSet` erased value.
///
/// Layout-backed `HashSet` facts use [`HashSetLoweringFact`] instead.  `Copy`
/// is not derived here because [`HashSetAbi`] carries an owned string for the
/// `Layout` variant.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct LoweringFact {
    pub kind: LoweringKind,
    pub element_type: HashSetElementType,
    pub abi_variant: HashSetAbi,
    pub drop_kind: DropKind,
}

/// Conversion failures when materializing scalar `HashSet` lowering facts.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LoweringFactError {
    UnresolvedHashSetElementType,
    UnsupportedHashSetElementType { ty: std::string::String },
}

impl LoweringFact {
    /// Materialize a `HashSet` lowering fact from the checker-resolved element type.
    ///
    /// # Errors
    ///
    /// Returns [`LoweringFactError::UnresolvedHashSetElementType`] when inference
    /// leaves the element unresolved at the checker boundary, and
    /// [`LoweringFactError::UnsupportedHashSetElementType`] for element types
    /// that the runtime lowering lane does not support.
    pub fn from_hashset_element_type(element_ty: &Ty) -> Result<Self, LoweringFactError> {
        if matches!(element_ty, Ty::Var(_) | Ty::Error) {
            return Err(LoweringFactError::UnresolvedHashSetElementType);
        }

        match element_ty.hashset_lowering_type_key() {
            Some(crate::ty::HashSetLoweringTypeKey::I64) => Ok(Self {
                kind: LoweringKind::HashSet,
                element_type: HashSetElementType::I64,
                abi_variant: HashSetAbi::Int64,
                drop_kind: DropKind::HashSetFree,
            }),
            Some(crate::ty::HashSetLoweringTypeKey::U64) => Ok(Self {
                kind: LoweringKind::HashSet,
                element_type: HashSetElementType::U64,
                abi_variant: HashSetAbi::Int64,
                drop_kind: DropKind::HashSetFree,
            }),
            Some(crate::ty::HashSetLoweringTypeKey::String) => Ok(Self {
                kind: LoweringKind::HashSet,
                element_type: HashSetElementType::Str,
                abi_variant: HashSetAbi::String,
                drop_kind: DropKind::HashSetFree,
            }),
            None => Err(LoweringFactError::UnsupportedHashSetElementType {
                ty: element_ty.user_facing().to_string(),
            }),
        }
    }
}

// ── HashMap lowering types ─────────────────────────────────────────────────────

/// Key-type discriminant for `HashMap` lowering facts.
///
/// Only `i64`, `u64`, and Copy named-record keys are admitted.  Other integer
/// widths, floats, strings, tuples, and managed types are rejected at the
/// admissibility gate.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum HashMapKeyType {
    I64,
    U64,
    /// Copy named-record key; size and alignment are carried in the owning fact.
    ///
    /// Full hash-eligibility is verified by the predicate in C-2a, wired in C-2c.
    Layout,
}

/// Value-type discriminant for `HashMap` lowering facts.
///
/// `Bool`, `Char`, and `Duration` are routed through the **existing scalar ABI
/// path** (same as `i32`/`i64`) per council Rev 4; they are not mapped to
/// `Layout`.  This preserves the existing value surface without regression.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum HashMapValueType {
    I32,
    I64,
    F64,
    Str,
    /// Routed through the existing scalar ABI path.  Council Rev 4 — must not regress.
    Bool,
    /// Routed through the existing scalar ABI path.  Council Rev 4 — must not regress.
    Char,
    /// Routed through the existing scalar ABI path.  Council Rev 4 — must not regress.
    Duration,
    /// Copy named-record value, opaque blob.  Wired in C-2c / C-3.
    Layout,
}

/// Runtime ABI selector for lowered `HashMap` operations.
///
/// `LayoutKey` is new in this slice and wired into admissibility in C-2c.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum HashMapAbi {
    /// Existing scalar `i64`/`u64` key ABI path.
    Int64Key,
    /// Existing string key ABI path.
    StringKey,
    /// Copy named-record key ABI backed by `hew_hashmap_*_layout`.  Wired in C-2c.
    LayoutKey {
        /// Name of the record type used as the map key (e.g. `"Point"`).
        key_record_name: std::string::String,
        /// Value type routing discriminant.
        val: HashMapValueType,
    },
}

/// Lifecycle state for checker-authored layout lowering facts.
///
/// Facts start [`Pending`](Self::Pending) (authored by the admissibility gate
/// in C-2c) and transition to [`Finalized`](Self::Finalized) after codegen
/// emits layout globals in C-3.  Codegen must never consume a `Finalized`
/// fact a second time.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum HashMapLoweringFactState {
    /// Authored by admissibility; not yet consumed by codegen.
    Pending,
    /// Codegen has emitted layout globals; size/align fields are populated.
    Finalized,
}

/// A checker-authored lowering fact for a `HashMap` call site.
///
/// `key_size`/`key_align` are `Some` for `LayoutKey` ABI and `None` for scalar
/// ABI variants where the size is implicit in the ABI.  After codegen
/// transitions the fact to `Finalized`, all layout-path size/align fields must
/// be `Some` — use [`assert_lowering_facts_consistent`] to verify.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct HashMapLoweringFact {
    pub abi: HashMapAbi,
    pub state: HashMapLoweringFactState,
    /// Key blob size in bytes; `None` for scalar key ABIs, `Some` for `LayoutKey`.
    pub key_size: Option<usize>,
    /// Key blob alignment in bytes; `None` for scalar key ABIs.
    pub key_align: Option<usize>,
    /// Value blob size in bytes; `None` for scalar value ABIs.
    pub val_size: Option<usize>,
    /// Value blob alignment in bytes; `None` for scalar value ABIs.
    pub val_align: Option<usize>,
}

/// A checker-authored lowering fact for a `HashSet` site using the layout ABI.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct HashSetLoweringFact {
    pub abi: HashSetAbi,
    pub state: HashMapLoweringFactState,
    /// Element blob size in bytes; `Some` for `HashSetAbi::Layout`, `None` otherwise.
    pub elem_size: Option<usize>,
    /// Element blob alignment in bytes; `Some` for `HashSetAbi::Layout`, `None` otherwise.
    pub elem_align: Option<usize>,
}

/// Errors returned when constructing `HashMap` lowering facts from resolved types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HashMapLoweringFactError {
    /// The candidate key type is not eligible as a `HashMap` layout key.
    ///
    /// Floats, managed types (`String`, `Bytes`), tuples, scalar integers other
    /// than `i64`/`u64`, and unresolved inference variables are all ineligible.
    IneligibleKeyType { ty: std::string::String },
    /// The candidate value type is not supported for `HashMap` lowering.
    UnsupportedValueType { ty: std::string::String },
}

/// Errors returned when constructing `HashSet` layout lowering facts from resolved types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HashSetLoweringFactError {
    /// The candidate element type is not eligible as a `HashSet` layout element.
    ///
    /// Floats, managed types (`String`, `Bytes`, `Duration`), and unresolved
    /// inference variables are ineligible.
    IneligibleElementType { ty: std::string::String },
}

/// Errors detected by [`assert_lowering_facts_consistent`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LoweringFactConsistencyError {
    /// A `LayoutKey` fact reached the consistency check in `Pending` state.
    ///
    /// Codegen must transition every `LayoutKey` fact to `Finalized` before
    /// the output-contract check runs.  A `Pending` fact at check time means
    /// codegen did not consume it — this is a codegen/checker contract
    /// violation.
    PendingLayoutKeyFact {
        /// Name of the record type whose fact is still pending.
        record_name: std::string::String,
    },
    /// A `Finalized` layout-key fact has `None` for `key_size`, or `key_size` is zero.
    FinalizedLayoutKeyMissingSize {
        /// Name of the record type whose fact is inconsistent.
        record_name: std::string::String,
    },
    /// A `Finalized` layout-key fact has a non-power-of-two `key_align`.
    FinalizedLayoutKeyBadAlign {
        /// Name of the record type whose fact is inconsistent.
        record_name: std::string::String,
        /// The bad alignment value.
        align: usize,
    },
    /// A `Finalized` `LayoutKey` fact with a `Layout` value type is missing
    /// `val_size` or `val_size` is zero.
    ///
    /// Codegen must not re-derive value layout from `ResolvedTy` (Rev 5
    /// codegen-abi-authority invariant); the checker must carry it explicitly.
    FinalizedLayoutValueMissingSize {
        /// Name of the key record type whose fact is inconsistent.
        record_name: std::string::String,
    },
    /// A `Finalized` `LayoutKey` fact with a `Layout` value type has a
    /// non-power-of-two `val_align`.
    FinalizedLayoutValueBadAlign {
        /// Name of the key record type whose fact is inconsistent.
        record_name: std::string::String,
        /// The bad alignment value.
        align: usize,
    },
}

impl HashMapKeyType {
    /// Derive the key-type discriminant from a checker-resolved type.
    ///
    /// This is the discriminant for the **scalar-key fast path** only. It maps
    /// `i64` (including `IntLiteral`), `u64`, and `Named` record types (which map
    /// to `Layout`). Everything else returns `IneligibleKeyType` here — but that
    /// verdict only means "not a scalar fast-path key", NOT "rejected".
    ///
    /// Bare `f64`/`f32` keys (and any other `Hash + Eq` key not on the fast
    /// path) are admitted by the bounds-based gate in `admissibility.rs`
    /// (`validate_hashmap_key_value_types`, `T: Hash + Eq`) and route through the
    /// **layout** key path, whose codegen hash/eq thunks compare/hash the float
    /// bit pattern (bitwise/total). So `HashMap<f64, V>` and a float-bearing
    /// record key both work; their float leaves use the same bitwise thunk.
    ///
    /// # Errors
    ///
    /// Returns [`HashMapLoweringFactError::IneligibleKeyType`] for any type not
    /// admitted as a `HashMap` key.
    pub fn from_ty(ty: &Ty) -> Result<Self, HashMapLoweringFactError> {
        match ty {
            Ty::I64 | Ty::IntLiteral => Ok(Self::I64),
            Ty::U64 => Ok(Self::U64),
            Ty::Named { .. } => Ok(Self::Layout),
            _ => Err(HashMapLoweringFactError::IneligibleKeyType {
                ty: ty.user_facing().to_string(),
            }),
        }
    }
}

impl HashMapValueType {
    /// Derive the value-type discriminant from a checker-resolved type.
    ///
    /// `Bool`, `Char`, and `Duration` are routed to the existing scalar ABI
    /// path per council Rev 4 — they are **not** `Layout`.
    ///
    /// # Errors
    ///
    /// Returns [`HashMapLoweringFactError::UnsupportedValueType`] for types not
    /// admitted as `HashMap` values.
    pub fn from_ty(ty: &Ty) -> Result<Self, HashMapLoweringFactError> {
        match ty {
            Ty::I32 => Ok(Self::I32),
            Ty::I64 | Ty::IntLiteral => Ok(Self::I64),
            Ty::F64 | Ty::FloatLiteral => Ok(Self::F64),
            Ty::String => Ok(Self::Str),
            Ty::Bool => Ok(Self::Bool),
            Ty::Char => Ok(Self::Char),
            Ty::Duration => Ok(Self::Duration),
            Ty::Named { .. } => Ok(Self::Layout),
            _ => Err(HashMapLoweringFactError::UnsupportedValueType {
                ty: ty.user_facing().to_string(),
            }),
        }
    }
}

/// Returns `Ok(())` when `elem_ty` passes the first-pass structural guard for
/// use as a `HashSet` layout element type.
///
/// This is a structural guard only; the full Copy-aggregate hash-eligibility
/// walk (C-2a) is wired in C-2c.  Floats, managed types (`String`, `Bytes`,
/// `Duration`), and unresolved variables are rejected here.
///
/// # Errors
///
/// Returns [`HashSetLoweringFactError::IneligibleElementType`] for any type
/// not structurally admissible as a `HashSet` layout element.
pub fn hashset_layout_element_admissible(ty: &Ty) -> Result<(), HashSetLoweringFactError> {
    match ty {
        // Integer, bool, char, and Named record types are structurally admissible.
        // C-2c applies hash-eligibility to reject records with float/managed fields.
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
        | Ty::Named { .. } => Ok(()),
        _ => Err(HashSetLoweringFactError::IneligibleElementType {
            ty: ty.user_facing().to_string(),
        }),
    }
}

/// Construct a `Pending` layout-key `HashMap` lowering fact for a **scalar**
/// value type.
///
/// `val_type` must not be [`HashMapValueType::Layout`]; use
/// [`hashmap_layout_key_layout_value_fact`] for record-typed values.  This
/// constructor sets `val_size`/`val_align` to `None` because scalar ABI value
/// sizes are implicit in the ABI; carrying them would be misleading.
///
/// # Panics
///
/// Panics when `val_type` is `HashMapValueType::Layout`.  Use
/// [`hashmap_layout_key_layout_value_fact`] for `Layout`-typed values.
#[must_use]
pub fn hashmap_layout_key_fact(
    key_record_name: std::string::String,
    key_size: usize,
    key_align: usize,
    val_type: HashMapValueType,
) -> HashMapLoweringFact {
    assert!(
        !matches!(val_type, HashMapValueType::Layout),
        "hashmap_layout_key_fact: val_type must not be Layout — \
         use hashmap_layout_key_layout_value_fact for record-typed values"
    );
    HashMapLoweringFact {
        abi: HashMapAbi::LayoutKey {
            key_record_name,
            val: val_type,
        },
        state: HashMapLoweringFactState::Pending,
        key_size: Some(key_size),
        key_align: Some(key_align),
        val_size: None,
        val_align: None,
    }
}

/// Construct a `Pending` layout-key `HashMap` lowering fact where the value
/// type is also a Copy named-record (`HashMapValueType::Layout`).
///
/// Both key and value sizes and alignments must be derived from record layouts
/// by the admissibility gate (C-2c) before calling this constructor.  Use
/// [`assert_lowering_facts_consistent`] after the fact transitions to
/// `Finalized` to verify all fields are non-zero and properly aligned.
#[must_use]
pub fn hashmap_layout_key_layout_value_fact(
    key_record_name: std::string::String,
    key_size: usize,
    key_align: usize,
    val_record_name: &str,
    val_size: usize,
    val_align: usize,
) -> HashMapLoweringFact {
    // val_record_name is accepted now so the C-3 consumer (which will store it
    // in a layout-global naming field) does not need a signature change.  It is
    // not yet carried in HashMapAbi because no field exists for it until C-3.
    let _ = val_record_name;
    HashMapLoweringFact {
        abi: HashMapAbi::LayoutKey {
            key_record_name,
            val: HashMapValueType::Layout,
        },
        state: HashMapLoweringFactState::Pending,
        key_size: Some(key_size),
        key_align: Some(key_align),
        val_size: Some(val_size),
        val_align: Some(val_align),
    }
}

/// Construct a `Pending` layout-element `HashSet` lowering fact with explicit
/// element size and alignment.
#[must_use]
pub fn hashset_layout_fact(
    elem_record_name: std::string::String,
    elem_size: usize,
    elem_align: usize,
) -> HashSetLoweringFact {
    HashSetLoweringFact {
        abi: HashSetAbi::Layout { elem_record_name },
        state: HashMapLoweringFactState::Pending,
        elem_size: Some(elem_size),
        elem_align: Some(elem_align),
    }
}

/// Verify the output contract for a slice of `HashMap` lowering facts.
///
/// This check is intended to be called by codegen after consuming all layout
/// facts — at that point every `LayoutKey` fact must be `Finalized`.  A
/// `Pending` fact reaching this check means codegen left a fact unconsumed,
/// which is a checker/codegen contract violation (Rev 5).
///
/// For every `Finalized` `LayoutKey` fact:
/// - `key_size` must be `Some` and nonzero.
/// - `key_align` must be `Some` and a power of two.
/// - When `val` is `HashMapValueType::Layout`: `val_size` must be `Some` and
///   nonzero, and `val_align` must be `Some` and a power of two.  Codegen must
///   not re-derive value layout from `ResolvedTy` (Rev 5 codegen-abi-authority).
///
/// Returns `Ok(())` when all facts are consistent, or the first
/// [`LoweringFactConsistencyError`] found.
///
/// # Errors
///
/// - [`LoweringFactConsistencyError::PendingLayoutKeyFact`] — a `LayoutKey`
///   fact is still `Pending` at check time (codegen did not consume it).
/// - [`LoweringFactConsistencyError::FinalizedLayoutKeyMissingSize`] — `None`
///   or zero `key_size`.
/// - [`LoweringFactConsistencyError::FinalizedLayoutKeyBadAlign`] — non-power-
///   of-two `key_align`.
/// - [`LoweringFactConsistencyError::FinalizedLayoutValueMissingSize`] — `None`
///   or zero `val_size` for a `Layout`-value fact.
/// - [`LoweringFactConsistencyError::FinalizedLayoutValueBadAlign`] — non-
///   power-of-two `val_align` for a `Layout`-value fact.
pub fn assert_lowering_facts_consistent(
    facts: &[HashMapLoweringFact],
) -> Result<(), LoweringFactConsistencyError> {
    for fact in facts {
        if let HashMapAbi::LayoutKey {
            key_record_name,
            val,
        } = &fact.abi
        {
            // Finding 1 (Rev 5): a Pending LayoutKey fact reaching this check
            // means codegen did not consume it — reject immediately.
            if fact.state == HashMapLoweringFactState::Pending {
                return Err(LoweringFactConsistencyError::PendingLayoutKeyFact {
                    record_name: key_record_name.clone(),
                });
            }

            // Validate key size/align.
            let key_size = fact.key_size.ok_or_else(|| {
                LoweringFactConsistencyError::FinalizedLayoutKeyMissingSize {
                    record_name: key_record_name.clone(),
                }
            })?;
            let key_align = fact.key_align.ok_or_else(|| {
                LoweringFactConsistencyError::FinalizedLayoutKeyMissingSize {
                    record_name: key_record_name.clone(),
                }
            })?;
            if key_size == 0 {
                return Err(
                    LoweringFactConsistencyError::FinalizedLayoutKeyMissingSize {
                        record_name: key_record_name.clone(),
                    },
                );
            }
            if !key_align.is_power_of_two() {
                return Err(LoweringFactConsistencyError::FinalizedLayoutKeyBadAlign {
                    record_name: key_record_name.clone(),
                    align: key_align,
                });
            }

            // Finding 2 (Rev 5): validate value size/align for Layout-value facts.
            // Codegen must not re-derive value layout from ResolvedTy.
            if matches!(val, HashMapValueType::Layout) {
                let val_size = fact.val_size.ok_or_else(|| {
                    LoweringFactConsistencyError::FinalizedLayoutValueMissingSize {
                        record_name: key_record_name.clone(),
                    }
                })?;
                let val_align = fact.val_align.ok_or_else(|| {
                    LoweringFactConsistencyError::FinalizedLayoutValueMissingSize {
                        record_name: key_record_name.clone(),
                    }
                })?;
                if val_size == 0 {
                    return Err(
                        LoweringFactConsistencyError::FinalizedLayoutValueMissingSize {
                            record_name: key_record_name.clone(),
                        },
                    );
                }
                if !val_align.is_power_of_two() {
                    return Err(LoweringFactConsistencyError::FinalizedLayoutValueBadAlign {
                        record_name: key_record_name.clone(),
                        align: val_align,
                    });
                }
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    // ── Existing scalar HashSet tests (unchanged) ─────────────────────────────

    #[test]
    fn hashset_i64_lowering_fact_uses_int64_abi() {
        let fact = LoweringFact::from_hashset_element_type(&Ty::I64)
            .expect("i64 HashSet facts should lower");
        assert_eq!(fact.kind, LoweringKind::HashSet);
        assert_eq!(fact.element_type, HashSetElementType::I64);
        assert_eq!(fact.abi_variant, HashSetAbi::Int64);
        assert_eq!(fact.drop_kind, DropKind::HashSetFree);
    }

    #[test]
    fn hashset_u64_lowering_fact_preserves_element_kind() {
        let fact = LoweringFact::from_hashset_element_type(&Ty::U64)
            .expect("u64 HashSet facts should lower");
        assert_eq!(fact.element_type, HashSetElementType::U64);
        assert_eq!(fact.abi_variant, HashSetAbi::Int64);
    }

    #[test]
    fn hashset_string_lowering_fact_uses_string_abi() {
        let fact = LoweringFact::from_hashset_element_type(&Ty::String)
            .expect("string HashSet facts should lower");
        assert_eq!(fact.element_type, HashSetElementType::Str);
        assert_eq!(fact.abi_variant, HashSetAbi::String);
    }

    #[test]
    fn hashset_lowering_fact_rejects_unresolved_elements() {
        assert_eq!(
            LoweringFact::from_hashset_element_type(&Ty::Var(crate::ty::TypeVar::fresh())),
            Err(LoweringFactError::UnresolvedHashSetElementType)
        );
    }

    // ── C-2b: HashMap layout key fact round-trip ──────────────────────────────

    #[test]
    fn hashmap_layout_key_lowering_fact_round_trips() {
        let fact = hashmap_layout_key_fact("Point".to_string(), 16, 8, HashMapValueType::I64);
        assert_eq!(fact.state, HashMapLoweringFactState::Pending);
        assert_eq!(fact.key_size, Some(16));
        assert_eq!(fact.key_align, Some(8));
        assert_eq!(fact.val_size, None);
        assert_eq!(fact.val_align, None);
        let HashMapAbi::LayoutKey {
            key_record_name,
            val,
        } = &fact.abi
        else {
            panic!("expected LayoutKey ABI");
        };
        assert_eq!(key_record_name, "Point");
        assert_eq!(*val, HashMapValueType::I64);
    }

    // ── C-2b: LoweringKind::HashMap serialization round-trip ─────────────────

    #[test]
    fn lowering_kind_hashmap_variant_round_trips() {
        let json = serde_json::to_string(&LoweringKind::HashMap)
            .expect("LoweringKind::HashMap should serialize");
        let back: LoweringKind =
            serde_json::from_str(&json).expect("LoweringKind::HashMap should deserialize");
        assert_eq!(back, LoweringKind::HashMap);
        // Verify snake_case wire form is stable.
        assert_eq!(json, r#""hash_map""#);
    }

    // ── C-2b: HashSet layout element fact ─────────────────────────────────────

    #[test]
    fn hashset_layout_abi_for_copy_record_element() {
        let fact = hashset_layout_fact("Point".to_string(), 16, 8);
        assert_eq!(fact.state, HashMapLoweringFactState::Pending);
        assert_eq!(fact.elem_size, Some(16));
        assert_eq!(fact.elem_align, Some(8));
        let HashSetAbi::Layout { elem_record_name } = &fact.abi else {
            panic!("expected Layout ABI");
        };
        assert_eq!(elem_record_name, "Point");
    }

    #[test]
    fn hashset_layout_abi_rejected_for_string_element() {
        let result = hashset_layout_element_admissible(&Ty::String);
        assert!(
            result.is_err(),
            "String is a managed type and must be rejected as a HashSet layout element"
        );
        assert_eq!(
            result.unwrap_err(),
            HashSetLoweringFactError::IneligibleElementType {
                ty: "string".to_string(),
            }
        );
    }

    // ── C-2b: HashMap key type validation ────────────────────────────────────

    #[test]
    fn hashmap_key_type_float_is_not_a_scalar_fast_path_key() {
        // `HashMapKeyType::from_ty` is the scalar fast-path classifier only.
        // A bare f64/f32 key is not on the scalar fast path (it has no scalar
        // key discriminant), so `from_ty` returns Err — but the key is still
        // ADMITTED elsewhere, via the `T: Hash + Eq` bounds gate, and routes
        // through the layout key path with the bitwise hash/eq thunk.
        let f64_result = HashMapKeyType::from_ty(&Ty::F64);
        assert!(
            f64_result.is_err(),
            "f64 is not a scalar fast-path key (it routes through the layout path)"
        );
        assert_eq!(
            f64_result.unwrap_err(),
            HashMapLoweringFactError::IneligibleKeyType {
                ty: "f64".to_string(),
            }
        );

        // f32 is also rejected.
        assert!(HashMapKeyType::from_ty(&Ty::F32).is_err());
    }

    // ── C-2b: bool/char/duration value types (council Rev 4) ─────────────────

    #[test]
    fn hashmap_layout_fact_includes_bool_char_duration_values() {
        let bool_val = HashMapValueType::from_ty(&Ty::Bool)
            .expect("Bool must be admitted as a HashMap value (council Rev 4)");
        assert_eq!(bool_val, HashMapValueType::Bool);

        let char_val = HashMapValueType::from_ty(&Ty::Char)
            .expect("Char must be admitted as a HashMap value (council Rev 4)");
        assert_eq!(char_val, HashMapValueType::Char);

        let dur_val = HashMapValueType::from_ty(&Ty::Duration)
            .expect("Duration must be admitted as a HashMap value (council Rev 4)");
        assert_eq!(dur_val, HashMapValueType::Duration);

        // These are on the scalar ABI path, not the Layout path.
        assert_ne!(bool_val, HashMapValueType::Layout);
        assert_ne!(char_val, HashMapValueType::Layout);
        assert_ne!(dur_val, HashMapValueType::Layout);
    }

    // ── C-2b rev: consistency check rejects finalized fact with missing size ───

    #[test]
    fn hashmap_fact_consistency_check_rejects_finalized_missing_size() {
        // A well-formed Pending fact produced by the helper.
        let pending = hashmap_layout_key_fact("Point".to_string(), 16, 8, HashMapValueType::I64);

        // Simulate codegen transitioning the fact to Finalized but forgetting
        // to populate key_size (a checker/codegen contract violation).
        let bad_finalized = HashMapLoweringFact {
            state: HashMapLoweringFactState::Finalized,
            key_size: None,
            ..pending
        };

        let result = assert_lowering_facts_consistent(&[bad_finalized]);
        assert_eq!(
            result,
            Err(
                LoweringFactConsistencyError::FinalizedLayoutKeyMissingSize {
                    record_name: "Point".to_string(),
                }
            ),
            "Finalized layout-key fact with None key_size must fail the consistency check"
        );
    }

    // ── C-2b rev: Finding 1 — Pending fact reaching check is rejected ─────────

    #[test]
    fn lowering_facts_consistency_rejects_pending_fact_reaching_check() {
        // A Pending LayoutKey fact (checker-authored, not yet consumed by codegen).
        let pending = hashmap_layout_key_fact("Point".to_string(), 16, 8, HashMapValueType::I64);
        assert_eq!(pending.state, HashMapLoweringFactState::Pending);

        // The consistency check must reject a Pending fact — if it reached the
        // check, codegen did not consume it, which is a contract violation.
        let result = assert_lowering_facts_consistent(&[pending]);
        assert_eq!(
            result,
            Err(LoweringFactConsistencyError::PendingLayoutKeyFact {
                record_name: "Point".to_string(),
            }),
            "A Pending LayoutKey fact reaching assert_lowering_facts_consistent \
             must be treated as a lifecycle contract violation (Rev 5)"
        );
    }

    // ── C-2b rev: Finding 2 — Layout-value facts must carry val_size/val_align ─

    #[test]
    fn hashmap_layout_value_fact_requires_val_size_align() {
        // The dedicated constructor carries both key and value layout.
        let fact = hashmap_layout_key_layout_value_fact("Point".to_string(), 16, 8, "Color", 8, 4);
        assert_eq!(fact.key_size, Some(16));
        assert_eq!(fact.key_align, Some(8));
        assert_eq!(fact.val_size, Some(8));
        assert_eq!(fact.val_align, Some(4));
        assert_eq!(fact.state, HashMapLoweringFactState::Pending);
        let HashMapAbi::LayoutKey { val, .. } = &fact.abi else {
            panic!("expected LayoutKey ABI");
        };
        assert_eq!(*val, HashMapValueType::Layout);

        // A finalized Layout-value fact WITH val_size/val_align passes.
        let finalized_ok = HashMapLoweringFact {
            state: HashMapLoweringFactState::Finalized,
            ..fact.clone()
        };
        assert!(
            assert_lowering_facts_consistent(&[finalized_ok]).is_ok(),
            "Finalized Layout-value fact with valid sizes must pass consistency check"
        );

        // A finalized Layout-value fact WITHOUT val_size must fail (Rev 5).
        let finalized_missing_val = HashMapLoweringFact {
            state: HashMapLoweringFactState::Finalized,
            val_size: None,
            ..fact
        };
        assert_eq!(
            assert_lowering_facts_consistent(&[finalized_missing_val]),
            Err(
                LoweringFactConsistencyError::FinalizedLayoutValueMissingSize {
                    record_name: "Point".to_string(),
                }
            ),
            "Finalized Layout-value fact with None val_size must fail the consistency check \
             (codegen must not re-derive value layout from ResolvedTy — Rev 5)"
        );
    }
}
