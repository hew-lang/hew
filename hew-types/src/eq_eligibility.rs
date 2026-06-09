//! Checker-side equality eligibility for layout-backed aggregate elements.
//!
//! This predicate is intentionally conservative: anything that would require
//! ownership-aware, floating-point, or heap-handle equality is rejected before a
//! later slice can route layout-backed `Vec::contains` through generated thunks.
//!
//! Named-type field traversal is shared with [`crate::hash_eligibility`] via
//! [`crate::eligibility_walker::walk_fields_for_eligibility`].

use crate::check::TypeDef;
use crate::eligibility_walker::walk_fields_for_eligibility;
use crate::ty::Ty;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum EqEligibility {
    Eligible,
    IneligibleFloat(Ty),
    IneligibleManaged(Ty),
    IneligibleOwned(Ty),
    IneligibleUnknown,
}

/// Returns `Some(rejection)` if `ty` is not equality-eligible, `None` if eligible.
///
/// This inner form is passed as a `fn` pointer to
/// [`walk_fields_for_eligibility`] so that named-type field recursion
/// can be shared with other eligibility predicates without code duplication.
fn eq_ineligibility(ty: &Ty, type_defs: &HashMap<String, TypeDef>) -> Option<EqEligibility> {
    match ty {
        Ty::I8
        | Ty::I16
        | Ty::I32
        | Ty::U8
        | Ty::U16
        | Ty::U32
        | Ty::I64
        | Ty::U64
        | Ty::Bool
        | Ty::Char
        | Ty::Unit => None,
        Ty::F32 | Ty::F64 => Some(EqEligibility::IneligibleFloat(ty.clone())),
        Ty::String => Some(EqEligibility::IneligibleManaged(ty.clone())),
        Ty::Tuple(elems) => elems
            .iter()
            .find_map(|elem| eq_ineligibility(elem, type_defs)),
        Ty::Named { name, .. } => match type_defs.get(name) {
            Some(type_def) if type_def.is_indirect => {
                Some(EqEligibility::IneligibleOwned(ty.clone()))
            }
            Some(type_def) => walk_fields_for_eligibility(type_def, type_defs, eq_ineligibility),
            None => Some(EqEligibility::IneligibleUnknown),
        },
        Ty::Var(_) | Ty::Error => Some(EqEligibility::IneligibleUnknown),
        Ty::IntLiteral | Ty::FloatLiteral | Ty::Never | Ty::Isize | Ty::Usize | Ty::Duration => {
            Some(EqEligibility::IneligibleUnknown)
        }
        Ty::Bytes
        | Ty::Array(_, _)
        | Ty::Slice(_)
        | Ty::Function { .. }
        | Ty::Closure { .. }
        | Ty::Pointer { .. }
        | Ty::TraitObject { .. }
        | Ty::Task(_)
        | Ty::AssocType { .. } => Some(EqEligibility::IneligibleOwned(ty.clone())),
    }
}

#[must_use]
pub(crate) fn ty_is_eq_eligible(ty: &Ty, type_defs: &HashMap<String, TypeDef>) -> EqEligibility {
    eq_ineligibility(ty, type_defs).unwrap_or(EqEligibility::Eligible)
}
