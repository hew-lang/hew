//! Independent verification of vector/map/set runtime calls and actor call signatures.

use super::{
    aggregate_glue, capability, clone_action_for_type, destroy_action_for_type, map_glue, set_glue,
    variant_glue, vector_glue, ActorOperation, BTreeMap, CallableId, OwnKind, PhysicalAggregateId,
    PhysicalCallable, PhysicalError, PhysicalGlueIds, PhysicalMapId, PhysicalMapOp, PhysicalModule,
    PhysicalSetId, PhysicalSetOp, PhysicalValueRecipe, PhysicalVariantId, PhysicalVectorGlue,
    PhysicalVectorId, PhysicalVectorOp, ResolvedTy, SemModule, SemParamPassing, SemTerminator,
    TypeInstanceKey, ValueCapability,
};

/// A removal returns the receiver and the element it took; a whole-buffer
/// drain returns the emptied receiver and the vector that moved out of it.
pub(crate) fn verify_vector_pair_result(
    module: &PhysicalModule,
    operation: PhysicalVectorOp,
    tuple: PhysicalAggregateId,
    glue: &PhysicalVectorGlue,
    result: &ResolvedTy,
) -> Result<(), PhysicalError> {
    let tuple = aggregate_glue(module, tuple)?;
    let value = if matches!(operation, PhysicalVectorOp::TakeAll { .. }) {
        tuple.fields.get(1).map(|field| &field.ty) == Some(&glue.ty)
    } else {
        tuple.fields.get(1) == Some(&glue.element)
    };
    if &tuple.ty != result
        || tuple.own != OwnKind::Owned
        || tuple.fields.len() != 2
        || tuple.fields[0].ty != glue.ty
        || !value
    {
        return Err(PhysicalError::new(
            "physical vector removal result descriptor is not its exact (Vec<T>, value) pair",
        ));
    }
    Ok(())
}

pub(crate) fn verify_vector_call(
    module: &PhysicalModule,
    operation: PhysicalVectorOp,
    id: PhysicalVectorId,
    arguments: &[ResolvedTy],
    result: &ResolvedTy,
) -> Result<(), PhysicalError> {
    let glue = vector_glue(module, id)?;
    if matches!(
        operation,
        PhysicalVectorOp::Index
            | PhysicalVectorOp::Get { .. }
            | PhysicalVectorOp::Slice
            | PhysicalVectorOp::SliceFrom
            | PhysicalVectorOp::Append
    ) && glue.element.clone.is_none()
    {
        return Err(PhysicalError::new(
            "vector read requires an element copy recipe",
        ));
    }
    let receiver = if operation == PhysicalVectorOp::New {
        result
    } else {
        arguments
            .first()
            .ok_or_else(|| PhysicalError::new("physical vector operation lacks its receiver"))?
    };
    if receiver != &glue.ty {
        return Err(PhysicalError::new(
            "physical vector operation uses a foreign vector descriptor",
        ));
    }
    match operation {
        PhysicalVectorOp::Contains => {
            if !module
                .value_capabilities
                .contains_key(&(glue.element.ty.clone(), ValueCapability::Eq))
            {
                return Err(PhysicalError::new(
                    "vector membership lacks its selected element equality",
                ));
            }
        }
        PhysicalVectorOp::Get { result: option }
        | PhysicalVectorOp::GetBorrow { result: option } => {
            let option = variant_glue(module, option)?;
            if &option.ty != result
                || option.is_indirect
                || option.variants.len() != 2
                || option.variants[0].fields.as_slice() != [glue.element.clone()]
                || !option.variants[1].fields.is_empty()
            {
                return Err(PhysicalError::new(
                    "physical vector get result descriptor is not its exact Some(T)/None value",
                ));
            }
        }
        PhysicalVectorOp::Pop { result: tuple }
        | PhysicalVectorOp::Remove { result: tuple }
        | PhysicalVectorOp::TakeFirst { result: tuple }
        | PhysicalVectorOp::TakeAll { result: tuple } => {
            verify_vector_pair_result(module, operation, tuple, glue, result)?;
        }
        PhysicalVectorOp::New
        | PhysicalVectorOp::Len
        | PhysicalVectorOp::Index
        | PhysicalVectorOp::Push
        | PhysicalVectorOp::Set
        | PhysicalVectorOp::Clear
        | PhysicalVectorOp::IndexBorrow => {}
        PhysicalVectorOp::Slice | PhysicalVectorOp::SliceFrom => {
            if result != &glue.ty {
                return Err(PhysicalError::new(
                    "physical vector slice result is not its own vector type",
                ));
            }
        }
        PhysicalVectorOp::Append => {
            if arguments.get(1) != Some(&glue.ty) || result != &glue.ty {
                return Err(PhysicalError::new(
                    "physical vector append joins a foreign vector type",
                ));
            }
        }
        PhysicalVectorOp::Join => {
            if glue.element.ty != ResolvedTy::String
                || arguments.get(1) != Some(&ResolvedTy::String)
                || result != &ResolvedTy::String
            {
                return Err(PhysicalError::new(
                    "physical vector join is not its exact Vec<string> to string contract",
                ));
            }
        }
    }
    Ok(())
}

pub(crate) fn verify_optional_value(
    module: &PhysicalModule,
    id: PhysicalVariantId,
    result: &ResolvedTy,
    value: &PhysicalValueRecipe,
) -> Result<(), PhysicalError> {
    let option = variant_glue(module, id)?;
    if &option.ty != result
        || option.is_indirect
        || option.variants.len() != 2
        || option.variants[0].fields.as_slice() != [value.clone()]
        || !option.variants[1].fields.is_empty()
    {
        return Err(PhysicalError::new(
            "physical optional descriptor is not its exact Some(T)/None value",
        ));
    }
    Ok(())
}

pub(crate) fn verify_map_call(
    module: &PhysicalModule,
    operation: PhysicalMapOp,
    id: PhysicalMapId,
    arguments: &[ResolvedTy],
    result: &ResolvedTy,
) -> Result<(), PhysicalError> {
    let glue = map_glue(module, id)?;
    let receiver = if operation == PhysicalMapOp::New {
        capability::require_key(module, &glue.key.ty)?;
        result
    } else {
        arguments
            .first()
            .ok_or_else(|| PhysicalError::new("map operation lacks its receiver"))?
    };
    if receiver != &glue.ty {
        return Err(PhysicalError::new(
            "physical map operation uses a foreign map descriptor",
        ));
    }
    match operation {
        PhysicalMapOp::Get { result: option } | PhysicalMapOp::GetBorrow { result: option } => {
            verify_optional_value(module, option, result, &glue.value)?;
        }
        PhysicalMapOp::Remove {
            result: pair,
            value: option,
        } => {
            let pair = aggregate_glue(module, pair)?;
            if &pair.ty != result || pair.fields.len() != 2 || pair.fields[0].ty != glue.ty {
                return Err(PhysicalError::new(
                    "map removal result is not its receiver/value pair",
                ));
            }
            verify_optional_value(module, option, &pair.fields[1].ty, &glue.value)?;
        }
        PhysicalMapOp::Entries { result: vector } => {
            let vector = vector_glue(module, vector)?;
            if &vector.ty != result
                || vector.element.ty
                    != ResolvedTy::Tuple(vec![glue.key.ty.clone(), glue.value.ty.clone()])
            {
                return Err(PhysicalError::new(
                    "map entries descriptor has the wrong key/value pair",
                ));
            }
        }
        PhysicalMapOp::New
        | PhysicalMapOp::Len
        | PhysicalMapOp::Index
        | PhysicalMapOp::ContainsKey
        | PhysicalMapOp::Insert
        | PhysicalMapOp::Clear
        | PhysicalMapOp::Keys
        | PhysicalMapOp::Values => {}
    }
    Ok(())
}

pub(crate) fn verify_set_call(
    module: &PhysicalModule,
    operation: PhysicalSetOp,
    id: PhysicalSetId,
    arguments: &[ResolvedTy],
    result: &ResolvedTy,
) -> Result<(), PhysicalError> {
    let glue = set_glue(module, id)?;
    let receiver = if operation == PhysicalSetOp::New {
        capability::require_key(module, &glue.element.ty)?;
        result
    } else {
        arguments
            .first()
            .ok_or_else(|| PhysicalError::new("set operation lacks its receiver"))?
    };
    if receiver != &glue.ty {
        return Err(PhysicalError::new(
            "physical set operation uses a foreign set descriptor",
        ));
    }
    if let PhysicalSetOp::Insert { result: pair } | PhysicalSetOp::Remove { result: pair } =
        operation
    {
        let pair = aggregate_glue(module, pair)?;
        if &pair.ty != result
            || pair.fields.len() != 2
            || pair.fields[0].ty != glue.ty
            || pair.fields[1].ty != ResolvedTy::Bool
        {
            return Err(PhysicalError::new(
                "set update result is not its receiver/presence pair",
            ));
        }
    }
    Ok(())
}

pub(crate) fn callable_for(
    module: &PhysicalModule,
    id: CallableId,
) -> Result<&PhysicalCallable, PhysicalError> {
    module
        .callables
        .get(id.0 as usize)
        .filter(|callable| callable.id == id)
        .ok_or_else(|| PhysicalError::new(format!("unknown physical callable {}", id.0)))
}

/// Project the checked actor protocol through this module's physical callables.
///
/// # Errors
/// Refuses missing actor declarations, handlers or initializer callables.
pub fn actor_signature(
    module: &PhysicalModule,
    operation: &hew_sir::ActorOperation,
) -> Result<hew_sir::SemSignature, PhysicalError> {
    operation
        .signature(&module.defs, &module.actors, &module.supervisors, |id| {
            module
                .callables
                .iter()
                .find(|callable| callable.id == id)
                .map(|callable| hew_sir::SemSignature {
                    params: callable
                        .params
                        .iter()
                        .map(|param| hew_sir::SemAbiParam {
                            ty: param.ty.clone(),
                            passing: param.passing,
                            caller_visible_projection: param.passing == SemParamPassing::BorrowMut,
                        })
                        .collect(),
                    return_ty: callable.return_ty.clone(),
                })
        })
        .map_err(PhysicalError::new)
}

pub(crate) fn actor_value_recipes(
    module: &SemModule,
    ids: &PhysicalGlueIds,
) -> Result<BTreeMap<ResolvedTy, PhysicalValueRecipe>, PhysicalError> {
    let mut types = Vec::new();
    for supervisor in &module.supervisors {
        types.extend(supervisor.config.iter().cloned());
    }
    for actor in &module.actors {
        types.push(actor.state_ty.clone());
        // A rejected completion returns its addressed target with the request.
        // Include the exact handle/role instances already admitted by SIR.
        types.extend(
            module
                .type_facts
                .keys()
                .filter(|key| actor.admits_target(&key.0))
                .map(|key| key.0.clone()),
        );
        types.extend(actor.fields.iter().map(|field| field.ty.clone()));
        for handler in &actor.handlers {
            if let Some(codec) = &handler.codec {
                for plan in codec.params.iter().chain(codec.reply.iter()) {
                    plan.visit_types(&mut |ty| types.push(ty.clone()));
                }
            }
            types.extend(handler.params.iter().cloned());
            types.push(handler.return_ty.clone());
        }
    }
    // Waiting requests retain their typed payload until admission.
    types.extend(module.functions.iter().flat_map(|function| {
        function
            .blocks
            .iter()
            .filter_map(|block| match &block.terminator {
                SemTerminator::ActorCall {
                    operation: ActorOperation::Submit { message_ty, .. },
                    ..
                } => Some(message_ty.clone()),
                SemTerminator::ActorCall {
                    operation: ActorOperation::StreamStart { actor, message, .. },
                    ..
                } => module
                    .actor(*actor)
                    .and_then(|actor| {
                        actor
                            .handlers
                            .iter()
                            .find(|handler| handler.message_id == *message)
                    })
                    .map(|handler| ResolvedTy::Tuple(handler.params.clone())),
                _ => None,
            })
    }));
    types
        .iter()
        .filter(|ty| **ty != ResolvedTy::Unit)
        .map(|ty| physical_value_recipe(module, ids, ty).map(|recipe| (ty.clone(), recipe)))
        .collect::<Result<_, _>>()
}

pub(crate) fn physical_value_recipe(
    module: &SemModule,
    ids: &PhysicalGlueIds,
    ty: &ResolvedTy,
) -> Result<PhysicalValueRecipe, PhysicalError> {
    let facts = module
        .type_facts
        .get(&TypeInstanceKey(ty.clone()))
        .ok_or_else(|| {
            PhysicalError::new(format!(
                "physical field `{}` has no semantic type facts",
                ty.user_facing()
            ))
        })?;
    let own = OwnKind::of_class(facts.class);
    Ok(PhysicalValueRecipe {
        ty: ty.clone(),
        own,
        clone: clone_action_for_type(ty, facts.clone, ids)?,
        destroy: if own == OwnKind::Owned {
            destroy_action_for_type(ty, ids)
        } else {
            None
        },
    })
}
