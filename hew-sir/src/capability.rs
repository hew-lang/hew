use hew_types::{
    ResolvedTy, TypeInstanceKey, ValueCapability, ValueMethodPlan, ValueMethodSelection,
};

use crate::{CallableId, CallableInstance, OwnKind, SemModule, SemParamPassing};

/// Executable semantic selection, independent of native callback layout.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemValueMethodPlan {
    /// Immutable evidence of the checker's choice for this exact type and operation.
    pub selection: ValueMethodSelection,
    /// Executable body demanded for a user selection; absent for structural operations.
    pub callable: Option<CallableId>,
}

/// Components traversed by a checker-authorized structural operation.
///
/// This identifies semantic fields, never padding or native storage bytes.
/// Each component requires its own selected implementation.
///
/// # Errors
/// Refuses a type without a structural operation in the executable value domain.
pub fn derived_capability_components(
    ty: &ResolvedTy,
    aggregates: &[crate::SemAggregateShape],
    variants: &[crate::SemVariantShape],
) -> Result<Vec<ResolvedTy>, String> {
    match ty {
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
        | ResolvedTy::String
        | ResolvedTy::Bytes => Ok(Vec::new()),
        ResolvedTy::Tuple(fields) => Ok(fields.clone()),
        _ => {
            if let Some((_, arguments)) = hew_types::runtime_call::collection_type_arguments(ty) {
                return Ok(arguments.to_vec());
            }
            if let Some(shape) = aggregates.iter().find(|shape| &shape.aggregate_ty == ty) {
                return Ok(shape.fields.iter().map(|field| field.ty.clone()).collect());
            }
            if let Some(shape) = variants.iter().find(|shape| &shape.enum_ty == ty) {
                return Ok(shape
                    .variants
                    .iter()
                    .flat_map(|variant| &variant.fields)
                    .map(|field| field.ty.clone())
                    .collect());
            }
            Err(format!(
                "`{}` has no semantic structural capability recipe",
                ty.user_facing()
            ))
        }
    }
}

pub(crate) fn verify_value_capability(
    module: &SemModule,
    ty: &ResolvedTy,
    capability: ValueCapability,
    plan: &SemValueMethodPlan,
) -> Result<(), String> {
    if plan.selection.ty() != ty || plan.selection.capability() != capability {
        return Err("selected operation belongs to another type or capability".to_string());
    }
    let facts = module
        .type_facts
        .get(&TypeInstanceKey(ty.clone()))
        .ok_or_else(|| "selected value capability has no concrete type facts".to_string())?;
    if !match capability {
        ValueCapability::Hash => facts.hash,
        ValueCapability::Eq => facts.eq,
    } {
        return Err("selected operation disagrees with checker capability facts".to_string());
    }
    let ValueMethodPlan::User {
        method: declaration,
        type_args,
    } = plan.selection.plan()
    else {
        if plan.callable.is_some() {
            return Err("derived operation carries an unselected callable".to_string());
        }
        for component in
            derived_capability_components(ty, &module.aggregate_shapes, &module.variant_shapes)?
        {
            if !module
                .value_capabilities
                .contains_key(&(component, capability))
            {
                return Err("derived operation lacks a selected component capability".to_string());
            }
        }
        return Ok(());
    };
    let callable = plan
        .callable
        .ok_or_else(|| "selected user operation has no executable callable".to_string())?;
    let selected = module.callable(callable).ok_or_else(|| {
        "selected capability callable is absent from the canonical table".to_string()
    })?;
    if &selected.declaration != declaration {
        return Err("selected capability declaration disagrees with its callable".to_string());
    }
    let exact_instance = match &selected.instance {
        CallableInstance::Closure(_) | CallableInstance::EntryAdapter => false,
        CallableInstance::Monomorphic => type_args.is_empty(),
        CallableInstance::Generic(key) => {
            &key.template.declaration == declaration && &key.type_args == type_args
        }
    };
    if !exact_instance {
        return Err("selected capability specialization disagrees with its callable".to_string());
    }
    verify_capability_signature(ty, capability, selected, *facts)
}

pub(crate) fn verify_capability_signature(
    ty: &ResolvedTy,
    capability: ValueCapability,
    callable: &crate::SemCallable,
    facts: hew_types::TypeFacts,
) -> Result<(), String> {
    let (arity, result) = match capability {
        ValueCapability::Hash => (1, ResolvedTy::I64),
        ValueCapability::Eq => (2, ResolvedTy::Bool),
    };
    let passing = if OwnKind::of_class(facts.class) == OwnKind::Owned {
        SemParamPassing::Borrow
    } else {
        SemParamPassing::ReadOnly
    };
    if callable.signature.return_ty != result
        || callable.signature.params.len() != arity
        || callable.signature.params.iter().any(|param| {
            &param.ty != ty || param.passing != passing || param.caller_visible_projection
        })
    {
        return Err(format!(
            "selected {capability:?} callable has an incompatible value signature"
        ));
    }
    Ok(())
}
