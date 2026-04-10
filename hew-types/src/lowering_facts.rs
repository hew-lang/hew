//! Checker-owned lowering metadata for erased runtime types.

use crate::Ty;
use serde::{Deserialize, Serialize};

/// High-level lowering lane identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum LoweringKind {
    HashSet,
}

/// Checker-authoritative `HashSet` element kinds that C++ lowering may consume.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum HashSetElementType {
    I64,
    U64,
    Str,
}

/// Runtime ABI selector for lowered `HashSet` operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum HashSetAbi {
    Int64,
    String,
}

/// Drop lane selector for lowered runtime handles.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DropKind {
    HashSetFree,
}

/// A single checker-owned lowering fact for an erased runtime value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct LoweringFact {
    pub kind: LoweringKind,
    pub element_type: HashSetElementType,
    pub abi_variant: HashSetAbi,
    pub drop_kind: DropKind,
}

/// Conversion failures when materializing lowering facts at the checker boundary.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LoweringFactError {
    UnresolvedHashSetElementType,
    UnsupportedHashSetElementType { ty: String },
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
        match element_ty {
            Ty::I64 | Ty::IntLiteral => Ok(Self {
                kind: LoweringKind::HashSet,
                element_type: HashSetElementType::I64,
                abi_variant: HashSetAbi::Int64,
                drop_kind: DropKind::HashSetFree,
            }),
            Ty::U64 => Ok(Self {
                kind: LoweringKind::HashSet,
                element_type: HashSetElementType::U64,
                abi_variant: HashSetAbi::Int64,
                drop_kind: DropKind::HashSetFree,
            }),
            Ty::String => Ok(Self {
                kind: LoweringKind::HashSet,
                element_type: HashSetElementType::Str,
                abi_variant: HashSetAbi::String,
                drop_kind: DropKind::HashSetFree,
            }),
            Ty::Var(_) | Ty::Error => Err(LoweringFactError::UnresolvedHashSetElementType),
            other => Err(LoweringFactError::UnsupportedHashSetElementType {
                ty: other.user_facing().to_string(),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
