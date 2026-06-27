//! Checker-side equality eligibility for layout-backed aggregate elements.
//!
//! This predicate is intentionally conservative: anything that would require
//! ownership-aware or heap-handle equality is rejected before a later slice can
//! route layout-backed `Vec::contains` through generated thunks.
//!
//! Floating-point fields (`f32`/`f64`) ARE eligible. Their structural equality
//! uses bitwise/total semantics — the codegen eq thunk bit-casts each float to
//! an integer of the same width and compares the bit patterns, so two NaN
//! values with identical bits compare equal and `+0.0` is distinct from `-0.0`.
//! This is reflexive (`x == x` for every `x`, including NaN), which is the
//! property structural equality needs to drive dedup, `contains`, and
//! `HashMap` keys correctly. It is deliberately different from the IEEE numeric `==` a
//! bare `f64 == f64` expression lowers to (where `NaN != NaN`).
//!
//! Named-type field traversal substitutes generic arguments before walking
//! fields so generic aggregates can defer parameter-dependent leaves until
//! monomorphization.

use crate::check::{TypeDef, TypeDefKind, VariantDef};
use crate::ty::Ty;
use crate::BuiltinType;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum EqEligibility {
    Eligible,
    IneligibleManaged(Ty),
    IneligibleOwned(Ty),
    IneligibleUnknown,
}

/// Returns `Some(rejection)` if `ty` is not equality-eligible, `None` if eligible.
///
/// This inner form takes the active generic type parameters so abstract
/// aggregates can admit parameter leaves and let monomorphized codegen resolve
/// the concrete equality path after substitution.
fn eq_ineligibility(
    ty: &Ty,
    type_defs: &HashMap<String, TypeDef>,
    type_params: &HashSet<String>,
) -> Option<EqEligibility> {
    match ty {
        Ty::I8
        | Ty::I16
        | Ty::I32
        | Ty::U8
        | Ty::U16
        | Ty::U32
        | Ty::I64
        | Ty::U64
        | Ty::Isize
        | Ty::Usize
        | Ty::Bool
        | Ty::Char
        | Ty::Duration
        | Ty::Unit
        // Floats are equality-eligible: codegen compares their bit patterns
        // (bitwise/total semantics — reflexive, NaN==NaN iff identical bits).
        | Ty::F32
        | Ty::F64
        | Ty::String => None,
        Ty::Tuple(elems) => elems
            .iter()
            .find_map(|elem| eq_ineligibility(elem, type_defs, type_params)),
        Ty::Named {
            builtin: Some(BuiltinType::Option | BuiltinType::Result),
            args,
            ..
        } => args
            .iter()
            .find_map(|arg| eq_ineligibility(arg, type_defs, type_params)),
        Ty::Named {
            builtin: Some(BuiltinType::Vec),
            args,
            ..
        } if args.len() == 1 => eq_ineligibility(&args[0], type_defs, type_params),
        Ty::Named {
            builtin: Some(BuiltinType::HashMap | BuiltinType::HashSet | BuiltinType::Rc),
            ..
        }
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
        Ty::Named { name, args, .. } => match type_defs.get(name).or_else(|| {
            name.split_once('.')
                .and_then(|(_, local)| type_defs.get(local))
        }) {
            Some(type_def) if type_def.is_indirect => {
                Some(EqEligibility::IneligibleOwned(ty.clone()))
            }
            Some(type_def) if type_def.kind == TypeDefKind::Enum => {
                walk_variants_for_eq_eligibility(type_def, args, type_defs, type_params)
            }
            Some(type_def) => {
                walk_instantiated_fields_for_eq_eligibility(type_def, args, type_defs, type_params)
            }
            None if args.is_empty() && type_params.contains(name) => None,
            None => Some(EqEligibility::IneligibleUnknown),
        },
        Ty::Var(_) | Ty::Error | Ty::IntLiteral | Ty::FloatLiteral | Ty::Never => {
            Some(EqEligibility::IneligibleUnknown)
        }
        Ty::Bytes => Some(EqEligibility::IneligibleManaged(ty.clone())),
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
    type_params: &HashSet<String>,
) -> Option<EqEligibility> {
    if type_args.is_empty() {
        let mut field_names: Vec<&String> = type_def.fields.keys().collect();
        field_names.sort();
        return field_names.into_iter().find_map(|name| {
            let field_ty = type_def.fields.get(name)?;
            eq_ineligibility(field_ty, type_defs, type_params)
        });
    }
    let mut field_names: Vec<&String> = type_def.fields.keys().collect();
    field_names.sort();
    field_names.into_iter().find_map(|name| {
        let field_ty = type_def.fields.get(name)?;
        let field_ty = instantiate_type_def_member(field_ty, &type_def.type_params, type_args);
        eq_ineligibility(&field_ty, type_defs, type_params)
    })
}

fn walk_variants_for_eq_eligibility(
    type_def: &TypeDef,
    type_args: &[Ty],
    type_defs: &HashMap<String, TypeDef>,
    type_params: &HashSet<String>,
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
                eq_ineligibility(&field_ty, type_defs, type_params)
            }),
            VariantDef::Struct(fields) => fields.iter().find_map(|(_, field_ty)| {
                let field_ty =
                    instantiate_type_def_member(field_ty, &type_def.type_params, type_args);
                eq_ineligibility(&field_ty, type_defs, type_params)
            }),
        }
    })
}

#[must_use]
pub(crate) fn ty_is_eq_eligible(ty: &Ty, type_defs: &HashMap<String, TypeDef>) -> EqEligibility {
    eq_ineligibility(ty, type_defs, &HashSet::new()).unwrap_or(EqEligibility::Eligible)
}

#[must_use]
pub(crate) fn ty_is_eq_eligible_with_type_params(
    ty: &Ty,
    type_defs: &HashMap<String, TypeDef>,
    type_params: &HashSet<String>,
) -> EqEligibility {
    eq_ineligibility(ty, type_defs, type_params).unwrap_or(EqEligibility::Eligible)
}
