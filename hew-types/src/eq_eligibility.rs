//! Checker-side equality eligibility for layout-backed aggregate elements.
//!
//! This predicate is intentionally conservative: anything that would require
//! ownership-aware, floating-point, or heap-handle equality is rejected before a
//! later slice can route layout-backed `Vec::contains` through generated thunks.
//!
//! Named-type field traversal is shared with [`crate::hash_eligibility`] via
//! [`crate::eligibility_walker::walk_fields_for_eligibility`].

use crate::check::{TypeDef, TypeDefKind, VariantDef};
use crate::eligibility_walker::walk_fields_for_eligibility;
use crate::ty::Ty;
use crate::BuiltinType;
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
        Ty::Named {
            builtin: Some(BuiltinType::Option | BuiltinType::Result),
            args,
            ..
        } => args.iter().find_map(|arg| eq_ineligibility(arg, type_defs)),
        Ty::Named { name, args, .. } => match type_defs.get(name).or_else(|| {
            name.split_once('.')
                .and_then(|(_, local)| type_defs.get(local))
        }) {
            Some(type_def) if type_def.is_indirect => {
                Some(EqEligibility::IneligibleOwned(ty.clone()))
            }
            Some(type_def) if type_def.kind == TypeDefKind::Enum => {
                walk_variants_for_eq_eligibility(type_def, args, type_defs)
            }
            Some(type_def) => {
                walk_instantiated_fields_for_eq_eligibility(type_def, args, type_defs)
            }
            None => Some(EqEligibility::IneligibleUnknown),
        },
        Ty::Var(_) | Ty::Error => Some(EqEligibility::IneligibleUnknown),
        Ty::IntLiteral | Ty::FloatLiteral | Ty::Never | Ty::Isize | Ty::Usize | Ty::Duration => {
            Some(EqEligibility::IneligibleUnknown)
        }
        Ty::Bytes
        | Ty::CancellationToken
        | Ty::Array(_, _)
        | Ty::Slice(_)
        | Ty::Function { .. }
        | Ty::Closure { .. }
        | Ty::Pointer { .. }
        | Ty::Borrow { .. }
        | Ty::TraitObject { .. }
        | Ty::Task(_)
        | Ty::AssocType { .. } => Some(EqEligibility::IneligibleOwned(ty.clone())),
    }
}

fn instantiate_type_def_member(ty: &Ty, type_params: &[String], type_args: &[Ty]) -> Ty {
    if type_params.is_empty() || type_params.len() != type_args.len() {
        return ty.clone();
    }
    let substitutions: HashMap<String, Ty> = type_params
        .iter()
        .cloned()
        .zip(type_args.iter().cloned())
        .collect();
    ty.substitute_named_params_parallel(&substitutions)
}

fn walk_instantiated_fields_for_eq_eligibility(
    type_def: &TypeDef,
    type_args: &[Ty],
    type_defs: &HashMap<String, TypeDef>,
) -> Option<EqEligibility> {
    if type_args.is_empty() {
        return walk_fields_for_eligibility(type_def, type_defs, eq_ineligibility);
    }
    let mut field_names: Vec<&String> = type_def.fields.keys().collect();
    field_names.sort();
    field_names.into_iter().find_map(|name| {
        let field_ty = type_def.fields.get(name)?;
        let field_ty = instantiate_type_def_member(field_ty, &type_def.type_params, type_args);
        eq_ineligibility(&field_ty, type_defs)
    })
}

fn walk_variants_for_eq_eligibility(
    type_def: &TypeDef,
    type_args: &[Ty],
    type_defs: &HashMap<String, TypeDef>,
) -> Option<EqEligibility> {
    let mut variant_names: Vec<&String> = type_def.variants.keys().collect();
    variant_names.sort();
    variant_names.into_iter().find_map(|name| {
        let variant = type_def.variants.get(name)?;
        match variant {
            VariantDef::Unit => None,
            VariantDef::Tuple(fields) => fields.iter().find_map(|field_ty| {
                let field_ty =
                    instantiate_type_def_member(field_ty, &type_def.type_params, type_args);
                eq_ineligibility(&field_ty, type_defs)
            }),
            VariantDef::Struct(fields) => fields.iter().find_map(|(_, field_ty)| {
                let field_ty =
                    instantiate_type_def_member(field_ty, &type_def.type_params, type_args);
                eq_ineligibility(&field_ty, type_defs)
            }),
        }
    })
}

#[must_use]
pub(crate) fn ty_is_eq_eligible(ty: &Ty, type_defs: &HashMap<String, TypeDef>) -> EqEligibility {
    eq_ineligibility(ty, type_defs).unwrap_or(EqEligibility::Eligible)
}
