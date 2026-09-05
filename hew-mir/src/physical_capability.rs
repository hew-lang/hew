use std::collections::BTreeMap;

use hew_types::{ResolvedTy, ValueCapability};

use super::{
    PhysicalAggregateId, PhysicalError, PhysicalGlueIds, PhysicalMapId, PhysicalModule,
    PhysicalSetId, PhysicalVariantId, PhysicalVectorId,
};

/// One concrete implementation of a checker-selected value operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PhysicalValueMethod {
    Scalar,
    String,
    Bytes,
    Aggregate(PhysicalAggregateId),
    Variant(PhysicalVariantId),
    Vector(PhysicalVectorId),
    Map(PhysicalMapId),
    Set(PhysicalSetId),
    User(hew_sir::CallableId),
}

/// The semantic selection and its checked concrete realization.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhysicalValueCapability {
    pub selection: hew_sir::SemValueMethodPlan,
    pub method: PhysicalValueMethod,
}

pub(super) fn build(
    module: &hew_sir::SemModule,
    ids: &PhysicalGlueIds,
) -> Result<BTreeMap<(ResolvedTy, ValueCapability), PhysicalValueCapability>, PhysicalError> {
    module
        .value_capabilities
        .iter()
        .map(|(key, selection)| {
            let method = match selection {
                hew_sir::SemValueMethodPlan::User { callable, .. } => {
                    PhysicalValueMethod::User(*callable)
                }
                hew_sir::SemValueMethodPlan::Derived => derived_method(&key.0, ids)?,
            };
            Ok((
                key.clone(),
                PhysicalValueCapability {
                    selection: selection.clone(),
                    method,
                },
            ))
        })
        .collect()
}

fn derived_method(
    ty: &ResolvedTy,
    ids: &PhysicalGlueIds,
) -> Result<PhysicalValueMethod, PhysicalError> {
    let method = match ty {
        ResolvedTy::String => PhysicalValueMethod::String,
        ResolvedTy::Bytes => PhysicalValueMethod::Bytes,
        _ if ids.aggregates.contains_key(ty) => PhysicalValueMethod::Aggregate(ids.aggregates[ty]),
        _ if ids.variants.contains_key(ty) => PhysicalValueMethod::Variant(ids.variants[ty]),
        _ if ids.vectors.contains_key(ty) => PhysicalValueMethod::Vector(ids.vectors[ty]),
        _ if ids.maps.contains_key(ty) => PhysicalValueMethod::Map(ids.maps[ty]),
        _ if ids.sets.contains_key(ty) => PhysicalValueMethod::Set(ids.sets[ty]),
        _ if is_scalar(ty) => PhysicalValueMethod::Scalar,
        _ => {
            return Err(PhysicalError::new(
                "selected capability has no concrete value recipe",
            ))
        }
    };
    Ok(method)
}

pub(super) fn verify(module: &PhysicalModule) -> Result<(), PhysicalError> {
    for ((ty, capability), plan) in &module.value_capabilities {
        let facts = super::semantic_type_facts(module, ty)?;
        let admitted = match capability {
            ValueCapability::Hash => facts.hash,
            ValueCapability::Eq => facts.eq,
        };
        if !admitted {
            return Err(PhysicalError::new(
                "physical value capability disagrees with its type facts",
            ));
        }
        if let hew_sir::SemValueMethodPlan::User { callable, .. } = &plan.selection {
            if plan.method != PhysicalValueMethod::User(*callable) {
                return Err(PhysicalError::new(
                    "physical value capability changed its selected callable",
                ));
            }
            verify_user(module, ty, *capability, *callable)?;
        } else {
            let components = derived_components(module, ty, plan.method)?;
            for component in components {
                if !module
                    .value_capabilities
                    .contains_key(&(component.clone(), *capability))
                {
                    return Err(PhysicalError::new(
                        "physical derived capability lacks its selected component",
                    ));
                }
            }
        }
    }
    Ok(())
}

fn derived_components<'a>(
    module: &'a PhysicalModule,
    ty: &ResolvedTy,
    method: PhysicalValueMethod,
) -> Result<Vec<&'a ResolvedTy>, PhysicalError> {
    let mismatch = || PhysicalError::new("physical derived capability uses another type's recipe");
    match method {
        PhysicalValueMethod::Scalar if is_scalar(ty) => Ok(vec![]),
        PhysicalValueMethod::String if *ty == ResolvedTy::String => Ok(vec![]),
        PhysicalValueMethod::Bytes if *ty == ResolvedTy::Bytes => Ok(vec![]),
        PhysicalValueMethod::Aggregate(id) => {
            let glue = super::aggregate_glue(module, id)?;
            if &glue.ty != ty {
                return Err(mismatch());
            }
            Ok(glue.fields.iter().map(|field| &field.ty).collect())
        }
        PhysicalValueMethod::Variant(id) => {
            let glue = super::variant_glue(module, id)?;
            if &glue.ty != ty {
                return Err(mismatch());
            }
            Ok(glue
                .variants
                .iter()
                .flat_map(|variant| &variant.fields)
                .map(|field| &field.ty)
                .collect())
        }
        PhysicalValueMethod::Vector(id) => {
            let glue = super::vector_glue(module, id)?;
            if &glue.ty != ty {
                return Err(mismatch());
            }
            Ok(vec![&glue.element.ty])
        }
        PhysicalValueMethod::Map(id) => {
            let glue = super::map_glue(module, id)?;
            if &glue.ty != ty {
                return Err(mismatch());
            }
            Ok(vec![&glue.key.ty, &glue.value.ty])
        }
        PhysicalValueMethod::Set(id) => {
            let glue = super::set_glue(module, id)?;
            if &glue.ty != ty {
                return Err(mismatch());
            }
            Ok(vec![&glue.element.ty])
        }
        _ => Err(mismatch()),
    }
}

fn verify_user(
    module: &PhysicalModule,
    ty: &ResolvedTy,
    capability: ValueCapability,
    id: hew_sir::CallableId,
) -> Result<(), PhysicalError> {
    let callable = module
        .callables
        .get(id.0 as usize)
        .filter(|callable| callable.id == id)
        .ok_or_else(|| PhysicalError::new("physical capability has no callable"))?;
    let (arity, return_ty) = match capability {
        ValueCapability::Hash => (1, ResolvedTy::I64),
        ValueCapability::Eq => (2, ResolvedTy::Bool),
    };
    let own = super::OwnKind::of_class(super::semantic_type_facts(module, ty)?.class);
    let passing = if own == super::OwnKind::Owned {
        hew_sir::SemParamPassing::Borrow
    } else {
        hew_sir::SemParamPassing::ReadOnly
    };
    if callable.return_ty != return_ty
        || callable.params.len() != arity
        || callable
            .params
            .iter()
            .any(|param| &param.ty != ty || param.passing != passing)
        || !module
            .functions
            .iter()
            .any(|function| function.callable == id)
    {
        return Err(PhysicalError::new(
            "physical capability callable lacks its exact signature or body",
        ));
    }
    Ok(())
}

fn is_scalar(ty: &ResolvedTy) -> bool {
    matches!(
        ty,
        ResolvedTy::Unit
            | ResolvedTy::Bool
            | ResolvedTy::Char
            | ResolvedTy::I8
            | ResolvedTy::I16
            | ResolvedTy::I32
            | ResolvedTy::I64
            | ResolvedTy::U8
            | ResolvedTy::U16
            | ResolvedTy::U32
            | ResolvedTy::U64
            | ResolvedTy::F32
            | ResolvedTy::F64
            | ResolvedTy::Duration
    )
}

pub(super) fn require_key(module: &PhysicalModule, ty: &ResolvedTy) -> Result<(), PhysicalError> {
    for capability in [ValueCapability::Hash, ValueCapability::Eq] {
        if !module
            .value_capabilities
            .contains_key(&(ty.clone(), capability))
        {
            return Err(PhysicalError::new(
                "collection construction lacks a selected key capability",
            ));
        }
    }
    Ok(())
}
