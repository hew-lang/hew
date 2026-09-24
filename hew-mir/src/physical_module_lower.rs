//! Module-level lowering from ownership SIR into physical MIR: glue construction and the type inventory.

use super::{
    actor_value_recipes, capability, collection_type_arguments, debug_field, debug_variant_field,
    physical_value_recipe, sequence_element_type, shared_handle_payload, structural, suspend,
    verify_physical_module, AggregateShapeRef, BTreeMap, BTreeSet, BlockId, BuiltinType,
    CallResult, CloneAction, CloneKind, DestroyAction, EncodingFormat, OwnKind, ParamCarrier,
    PhysicalAggregateDescriptor, PhysicalAggregateGlue, PhysicalAggregateId, PhysicalBlock,
    PhysicalCallSignature, PhysicalCallable, PhysicalClosure, PhysicalDebug, PhysicalDebugFunction,
    PhysicalDebugLocal, PhysicalDebugVariant, PhysicalEnvironmentGlue, PhysicalError,
    PhysicalFunction, PhysicalLayout, PhysicalMapDescriptor, PhysicalMapGlue, PhysicalMapId,
    PhysicalModule, PhysicalParam, PhysicalRepr, PhysicalResourceDescriptor, PhysicalResourceId,
    PhysicalSetDescriptor, PhysicalSetGlue, PhysicalSetId, PhysicalSharedDescriptor,
    PhysicalSharedGlue, PhysicalSharedId, PhysicalStorage, PhysicalTarget, PhysicalTerminator,
    PhysicalTypeInventory, PhysicalVariantCase, PhysicalVariantDescriptor, PhysicalVariantGlue,
    PhysicalVariantId, PhysicalVectorDescriptor, PhysicalVectorGlue, PhysicalVectorId,
    PhysicalVtable, PhysicalVtableId, PhysicalVtableSlot, ReleaseEffects, ResolvedTy, SemFunction,
    SemModule, SemTerminator, StorageId, StorageOrigin, TypeInstanceKey, ValueId,
    VerifiedPhysicalModule,
};

/// Lower verified ownership SIR into the sole target-realized MIR.
///
/// This boundary re-runs SIR verification, resolves every type layout and
/// explicit ownership action, then verifies the resulting storage/CFG model.
/// Code generation accepts only the returned immutable wrapper.
///
/// # Errors
///
/// Returns [`PhysicalError`] when SIR verification fails, a concrete target
/// layout is absent, an ownership action has no admitted physical realization,
/// or the resulting storage/CFG model violates the physical verifier.
#[expect(
    clippy::too_many_lines,
    reason = "materializes the complete checked module and its callable ABI"
)]
pub fn lower_physical_module(
    module: &SemModule,
    target: PhysicalTarget,
) -> Result<VerifiedPhysicalModule, PhysicalError> {
    let checked = hew_sir::check_module(module).map_err(|diagnostics| {
        PhysicalError::new(format!(
            "SIR verification failed before physical lowering: {:?}",
            diagnostics[0].kind
        ))
    })?;

    let PhysicalGlue {
        resources,
        environment_glue,
        aggregate_glue,
        variant_glue,
        vector_glue,
        map_glue,
        set_glue,
        shared_glue,
        ids,
    } = build_glue(module)?;

    let resumable = suspend::semantic_callables(&checked);
    let callables = module
        .callables
        .iter()
        .map(|callable| {
            let params = callable
                .signature
                .params
                .iter()
                .map(|param| {
                    Ok(PhysicalParam {
                        ty: param.ty.clone(),
                        layout: required_layout(&target, &param.ty)?.clone(),
                        passing: param.passing,
                        carrier: param_carrier(param.passing, required_layout(&target, &param.ty)?),
                    })
                })
                .collect::<Result<Vec<_>, PhysicalError>>()?;
            let return_layout = if callable.signature.return_ty == ResolvedTy::Never
                || (callable.signature.return_ty == ResolvedTy::Unit
                    && !module.closures.iter().any(|closure| {
                        closure.body == callable.id && closure.generator_yield.is_some()
                    })) {
                None
            } else {
                Some(required_layout(&target, &callable.signature.return_ty)?.clone())
            };
            Ok(PhysicalCallable {
                id: callable.id,
                declaration: callable.declaration.clone(),
                instance: callable.instance.clone(),
                symbol: callable.symbol.clone(),
                params,
                return_ty: callable.signature.return_ty.clone(),
                return_layout,
                is_resumable: resumable.contains(&callable.id),
                receiver_handback: callable.signature.hands_back_receiver(),
            })
        })
        .collect::<Result<Vec<_>, PhysicalError>>()?;

    let mut debug = PhysicalDebug {
        scopes: module.debug.scopes.clone(),
        functions: BTreeMap::new(),
        records: module
            .aggregate_shapes
            .iter()
            .map(|shape| {
                (
                    shape.aggregate_ty.clone(),
                    shape.fields.iter().map(debug_field).collect(),
                )
            })
            .collect(),
        enums: module
            .variant_shapes
            .iter()
            .map(|shape| {
                (
                    shape.enum_ty.clone(),
                    shape
                        .variants
                        .iter()
                        .map(|variant| PhysicalDebugVariant {
                            name: variant.name.clone(),
                            fields: variant.fields.iter().map(debug_variant_field).collect(),
                        })
                        .collect(),
                )
            })
            .collect(),
    };
    // Rendering recipes are interned as the render sites are lowered, so the
    // table holds exactly the types this module renders.
    let structural = std::cell::RefCell::new(structural::StructuralGlue::default());
    let functions = module
        .functions
        .iter()
        .map(|function| {
            let certificate = checked
                .function(function.callable)
                .ok_or_else(|| PhysicalError::new("physical function lacks its SIR certificate"))?;
            let (lowered, attribution) =
                lower_function(module, &target, function, &ids, &structural, certificate)?;
            if let Some(attribution) = attribution {
                debug.functions.insert(function.callable, attribution);
            }
            Ok(lowered)
        })
        .collect::<Result<Vec<_>, PhysicalError>>()?;
    let mut structural_glue = structural.into_inner().finish()?;
    for index in 0..structural_glue.len() {
        let callees = structural::display_callees(&structural_glue, structural_glue[index].id)?;
        structural_glue[index].is_resumable =
            callees.iter().any(|callee| resumable.contains(callee));
    }

    let vtables = module
        .vtables
        .iter()
        .map(|vtable| {
            let slots = vtable
                .slots
                .iter()
                .map(|slot| {
                    let params = slot
                        .signature
                        .params
                        .iter()
                        .map(|param| {
                            let layout = required_layout(&target, &param.ty)?.clone();
                            Ok(PhysicalParam {
                                ty: param.ty.clone(),
                                carrier: param_carrier(param.passing, &layout),
                                passing: param.passing,
                                layout,
                            })
                        })
                        .collect::<Result<Vec<_>, PhysicalError>>()?;
                    let return_layout = if slot.signature.return_ty == ResolvedTy::Never
                        || slot.signature.return_ty == ResolvedTy::Unit
                    {
                        None
                    } else {
                        Some(required_layout(&target, &slot.signature.return_ty)?.clone())
                    };
                    Ok(PhysicalVtableSlot {
                        slot: slot.slot,
                        callee: slot.callee,
                        signature: PhysicalCallSignature {
                            params,
                            return_ty: slot.signature.return_ty.clone(),
                            return_layout,
                        },
                    })
                })
                .collect::<Result<Vec<_>, PhysicalError>>()?;
            Ok(PhysicalVtable {
                id: PhysicalVtableId(vtable.id.0),
                dyn_ty: vtable.dyn_ty.clone(),
                concrete_ty: vtable.concrete_ty.clone(),
                concrete_layout: required_layout(&target, &vtable.concrete_ty)?.clone(),
                concrete: physical_value_recipe(module, &ids, &vtable.concrete_ty)?,
                slots,
            })
        })
        .collect::<Result<Vec<_>, PhysicalError>>()?;

    let mut physical = PhysicalModule {
        releases: ReleaseEffects::default(),
        actor_recipes: actor_value_recipes(module, &ids)?,
        actors: module.actors.clone(),
        supervisors: module.supervisors.clone(),
        resources,
        value_capabilities: capability::build(module, &ids)?,
        closures: module
            .closures
            .iter()
            .map(|closure| PhysicalClosure {
                generator_yield: closure.generator_yield.clone(),
                id: closure.id,
                body: closure.body,
                ty: closure.ty.clone(),
            })
            .collect(),
        vtables,
        environment_glue,
        target,
        aggregate_glue,
        variant_glue,
        vector_glue,
        map_glue,
        set_glue,
        shared_glue,
        structural_glue,
        type_facts: module.type_facts.clone(),
        callables,
        functions,
        entry_callable: module.entry_callable,
        entry_exit_plan: module.entry_exit_plan.clone(),
        string_literals: module.string_literals.clone(),
        bytes_literals: module.bytes_literals.clone(),
        regex_patterns: module.regex_patterns.clone(),
        debug,
    };
    let callback_resumption = physical
        .value_capabilities
        .keys()
        .map(|key| {
            Ok((
                key.clone(),
                capability::callees(&physical, &key.0, key.1)?
                    .iter()
                    .any(|callee| resumable.contains(callee)),
            ))
        })
        .collect::<Result<Vec<_>, PhysicalError>>()?;
    for (key, is_resumable) in callback_resumption {
        physical
            .value_capabilities
            .get_mut(&key)
            .ok_or_else(|| PhysicalError::new("selected callback disappeared during lowering"))?
            .is_resumable = is_resumable;
    }
    physical.releases = ReleaseEffects::compute(&physical);
    verify_physical_module(&physical)?;
    Ok(VerifiedPhysicalModule(physical))
}

/// One index for resolving type-directed actions to their concrete recipes.
pub(crate) struct PhysicalGlueIds {
    pub(crate) resources: BTreeMap<ResolvedTy, PhysicalResourceId>,
    pub(crate) aggregates: BTreeMap<ResolvedTy, PhysicalAggregateId>,
    pub(crate) variants: BTreeMap<ResolvedTy, PhysicalVariantId>,
    pub(crate) vectors: BTreeMap<ResolvedTy, PhysicalVectorId>,
    pub(crate) maps: BTreeMap<ResolvedTy, PhysicalMapId>,
    pub(crate) sets: BTreeMap<ResolvedTy, PhysicalSetId>,
    pub(crate) shared: BTreeMap<ResolvedTy, PhysicalSharedId>,
}

pub(crate) struct PhysicalGlue {
    resources: Vec<PhysicalResourceDescriptor>,
    environment_glue: Vec<PhysicalEnvironmentGlue>,
    aggregate_glue: Vec<PhysicalAggregateGlue>,
    variant_glue: Vec<PhysicalVariantGlue>,
    vector_glue: Vec<PhysicalVectorGlue>,
    map_glue: Vec<PhysicalMapGlue>,
    set_glue: Vec<PhysicalSetGlue>,
    shared_glue: Vec<PhysicalSharedGlue>,
    ids: PhysicalGlueIds,
}

#[allow(
    clippy::too_many_lines,
    reason = "value recipes are resolved together so recursive actions share one identity index"
)]
pub(crate) fn build_glue(module: &SemModule) -> Result<PhysicalGlue, PhysicalError> {
    let inventory = physical_type_inventory(module);
    let aggregates = inventory
        .aggregates()
        .map(|aggregate| {
            OwnKind::of_ty(&aggregate.ty, &module.type_facts)
                .map(|own| (aggregate, own))
                .map_err(PhysicalError::new)
        })
        .collect::<Result<Vec<_>, _>>()?;
    let variants = inventory
        .variants()
        .map(|variant| {
            OwnKind::of_ty(&variant.ty, &module.type_facts)
                .map(|own| (variant, own))
                .map_err(PhysicalError::new)
        })
        .collect::<Result<Vec<_>, _>>()?;
    let aggregate_ids = aggregates
        .iter()
        .enumerate()
        .map(|(index, (aggregate, _))| {
            let index = u32::try_from(index)
                .map_err(|_| PhysicalError::new("physical aggregate count exceeds u32"))?;
            Ok((aggregate.ty.clone(), PhysicalAggregateId(index)))
        })
        .collect::<Result<BTreeMap<_, _>, PhysicalError>>()?;
    let variant_ids = variants
        .iter()
        .enumerate()
        .map(|(index, (variant, _))| {
            let index = u32::try_from(index)
                .map_err(|_| PhysicalError::new("physical variant count exceeds u32"))?;
            Ok((variant.ty.clone(), PhysicalVariantId(index)))
        })
        .collect::<Result<BTreeMap<_, _>, PhysicalError>>()?;
    let vector_ids = inventory
        .vectors()
        .enumerate()
        .map(|(index, vector)| {
            let index = u32::try_from(index)
                .map_err(|_| PhysicalError::new("physical vector count exceeds u32"))?;
            Ok((vector.ty.clone(), PhysicalVectorId(index)))
        })
        .collect::<Result<BTreeMap<_, _>, PhysicalError>>()?;
    let map_ids = inventory
        .maps()
        .enumerate()
        .map(|(index, map)| {
            let index = u32::try_from(index)
                .map_err(|_| PhysicalError::new("physical map count exceeds u32"))?;
            Ok((map.ty.clone(), PhysicalMapId(index)))
        })
        .collect::<Result<BTreeMap<_, _>, PhysicalError>>()?;
    let set_ids = inventory
        .sets()
        .enumerate()
        .map(|(index, set)| {
            let index = u32::try_from(index)
                .map_err(|_| PhysicalError::new("physical set count exceeds u32"))?;
            Ok((set.ty.clone(), PhysicalSetId(index)))
        })
        .collect::<Result<BTreeMap<_, _>, PhysicalError>>()?;
    let shared_ids = inventory
        .shared()
        .enumerate()
        .map(|(index, shared)| {
            let index = u32::try_from(index)
                .map_err(|_| PhysicalError::new("physical shared handle count exceeds u32"))?;
            Ok((shared.ty.clone(), PhysicalSharedId(index)))
        })
        .collect::<Result<BTreeMap<_, _>, PhysicalError>>()?;
    let resources = inventory
        .resources()
        .enumerate()
        .map(|(index, resource)| {
            let index = u32::try_from(index)
                .map_err(|_| PhysicalError::new("physical resource count exceeds u32"))?;
            Ok((resource.ty.clone(), PhysicalResourceId(index)))
        })
        .collect::<Result<BTreeMap<_, _>, PhysicalError>>()?;
    let ids = PhysicalGlueIds {
        resources,
        aggregates: aggregate_ids,
        variants: variant_ids,
        vectors: vector_ids,
        maps: map_ids,
        sets: set_ids,
        shared: shared_ids,
    };
    let value_recipe = |ty: &ResolvedTy| physical_value_recipe(module, &ids, ty);
    let aggregate_glue = aggregates
        .into_iter()
        .map(|(aggregate, own)| {
            let id = ids.aggregates[&aggregate.ty];
            let shape = aggregate_shape_ref(module, &aggregate.ty)?;
            let recipes = hew_sir::aggregate_field_recipes(
                shape,
                &aggregate.ty,
                &module.aggregate_shapes,
                &module.type_facts,
            )
            .map_err(PhysicalError::new)?;
            let fields = recipes
                .iter()
                .map(|recipe| value_recipe(&recipe.ty))
                .collect::<Result<Vec<_>, PhysicalError>>()?;
            Ok(PhysicalAggregateGlue {
                id,
                ty: aggregate.ty.clone(),
                own,
                fields,
            })
        })
        .collect::<Result<Vec<_>, PhysicalError>>()?;
    let variant_glue = variants
        .into_iter()
        .map(|(descriptor, own)| {
            let shape = module
                .variant_shape_for_type(&descriptor.ty)
                .ok_or_else(|| {
                    PhysicalError::new(format!(
                        "variant `{}` has no exact SIR descriptor",
                        descriptor.ty.user_facing()
                    ))
                })?;
            let variants = shape
                .variants
                .iter()
                .enumerate()
                .map(|(index, _)| {
                    let index = u32::try_from(index)
                        .map_err(|_| PhysicalError::new("variant index exceeds u32"))?;
                    let recipes = hew_sir::variant_field_recipes(
                        shape.id,
                        index,
                        &descriptor.ty,
                        &module.variant_shapes,
                        &module.type_facts,
                    )
                    .map_err(PhysicalError::new)?;
                    let fields = recipes
                        .iter()
                        .map(|recipe| value_recipe(&recipe.ty))
                        .collect::<Result<Vec<_>, PhysicalError>>()?;
                    Ok(PhysicalVariantCase { fields })
                })
                .collect::<Result<Vec<_>, PhysicalError>>()?;
            Ok(PhysicalVariantGlue {
                id: ids.variants[&descriptor.ty],
                ty: descriptor.ty.clone(),
                own,
                is_indirect: descriptor.is_indirect,
                variants,
            })
        })
        .collect::<Result<Vec<_>, PhysicalError>>()?;
    let vector_glue = inventory
        .vectors()
        .map(|descriptor| {
            let element = value_recipe(&descriptor.element)?;
            if element.own == OwnKind::Owned && element.destroy.is_none() {
                return Err(PhysicalError::new(format!(
                    "vector element `{}` lacks a complete value recipe",
                    descriptor.element.user_facing()
                )));
            }
            Ok(PhysicalVectorGlue {
                id: ids.vectors[&descriptor.ty],
                ty: descriptor.ty.clone(),
                element,
            })
        })
        .collect::<Result<Vec<_>, PhysicalError>>()?;
    let map_glue = inventory
        .maps()
        .map(|descriptor| {
            Ok(PhysicalMapGlue {
                id: ids.maps[&descriptor.ty],
                ty: descriptor.ty.clone(),
                key: value_recipe(&descriptor.key)?,
                value: value_recipe(&descriptor.value)?,
            })
        })
        .collect::<Result<Vec<_>, PhysicalError>>()?;
    let set_glue = inventory
        .sets()
        .map(|descriptor| {
            Ok(PhysicalSetGlue {
                id: ids.sets[&descriptor.ty],
                ty: descriptor.ty.clone(),
                element: value_recipe(&descriptor.element)?,
            })
        })
        .collect::<Result<Vec<_>, PhysicalError>>()?;
    let shared_glue = inventory
        .shared()
        .map(|descriptor| {
            let payload = value_recipe(&descriptor.payload)?;
            if payload.own == OwnKind::Owned && payload.destroy.is_none() {
                return Err(PhysicalError::new(format!(
                    "shared payload `{}` lacks a complete value recipe",
                    descriptor.payload.user_facing()
                )));
            }
            Ok(PhysicalSharedGlue {
                id: ids.shared[&descriptor.ty],
                ty: descriptor.ty.clone(),
                payload,
            })
        })
        .collect::<Result<Vec<_>, PhysicalError>>()?;
    let environment_glue = inventory
        .types()
        .filter_map(|ty| {
            let captures = match ty {
                ResolvedTy::Closure { captures, .. } => captures.as_slice(),
                ResolvedTy::Function { .. } => &[],
                _ => return None,
            };
            Some((|| {
                let facts = module
                    .type_facts
                    .get(&TypeInstanceKey(ty.clone()))
                    .ok_or_else(|| {
                        PhysicalError::new("callable environment lacks concrete type facts")
                    })?;
                if !matches!(
                    facts.clone,
                    CloneKind::None | CloneKind::DeepCopy | CloneKind::FieldWise
                ) {
                    return Err(PhysicalError::new(
                        "callable environment requires independent-copy type facts",
                    ));
                }
                Ok(PhysicalEnvironmentGlue {
                    ty: ty.clone(),
                    fields: captures
                        .iter()
                        .map(value_recipe)
                        .collect::<Result<Vec<_>, PhysicalError>>()?,
                    cloneable: facts.clone != CloneKind::None,
                })
            })())
        })
        .collect::<Result<Vec<_>, PhysicalError>>()?;
    Ok(PhysicalGlue {
        resources: inventory.resources().cloned().collect(),
        environment_glue,
        aggregate_glue,
        variant_glue,
        vector_glue,
        map_glue,
        set_glue,
        shared_glue,
        ids,
    })
}

pub(crate) fn aggregate_shape_ref(
    module: &SemModule,
    ty: &ResolvedTy,
) -> Result<AggregateShapeRef, PhysicalError> {
    match ty {
        ResolvedTy::Tuple(_) => Ok(AggregateShapeRef::Tuple),
        _ => module
            .aggregate_shape_for_type(ty)
            .map(|shape| AggregateShapeRef::Record(shape.id))
            .ok_or_else(|| {
                PhysicalError::new(format!(
                    "aggregate `{}` has no exact SIR shape descriptor",
                    ty.user_facing()
                ))
            }),
    }
}

pub(crate) fn clone_action_for_type(
    ty: &ResolvedTy,
    clone: CloneKind,
    ids: &PhysicalGlueIds,
) -> Result<Option<CloneAction>, PhysicalError> {
    let action = match clone {
        CloneKind::None => return Ok(None),
        CloneKind::Bits => CloneAction::Bitwise,
        CloneKind::Retain if ty == &ResolvedTy::String => CloneAction::StringRetain,
        CloneKind::Retain if ty == &ResolvedTy::Bytes => CloneAction::BytesRetain,
        CloneKind::Retain if ty.is_builtin(BuiltinType::Rc) => CloneAction::RcRetain,
        CloneKind::Retain if ty.is_builtin(BuiltinType::Weak) => CloneAction::WeakRetain,
        CloneKind::DeepCopy if encoding_format(ty).is_some() => {
            CloneAction::Encoding(encoding_format(ty).expect("checked encoding receiver"))
        }
        CloneKind::DeepCopy | CloneKind::FieldWise
            if matches!(ty, ResolvedTy::Function { .. } | ResolvedTy::Closure { .. }) =>
        {
            CloneAction::Callable
        }
        CloneKind::DeepCopy | CloneKind::FieldWise if ids.vectors.contains_key(ty) => {
            if matches!(ty, ResolvedTy::Array(_, _)) {
                CloneAction::Array(ids.vectors[ty])
            } else {
                CloneAction::Vector(ids.vectors[ty])
            }
        }
        CloneKind::DeepCopy | CloneKind::FieldWise if ids.maps.contains_key(ty) => {
            CloneAction::Map(ids.maps[ty])
        }
        CloneKind::DeepCopy | CloneKind::FieldWise if ids.sets.contains_key(ty) => {
            CloneAction::Set(ids.sets[ty])
        }
        CloneKind::FieldWise if ids.aggregates.contains_key(ty) => {
            CloneAction::Aggregate(ids.aggregates[ty])
        }
        CloneKind::FieldWise if ids.variants.contains_key(ty) => {
            CloneAction::Variant(ids.variants[ty])
        }
        CloneKind::FieldWise => {
            return Err(PhysicalError::new(format!(
                "field-wise value `{}` has no demanded physical descriptor",
                ty.user_facing()
            )));
        }
        unsupported => {
            return Err(PhysicalError::new(format!(
                "physical clone action for `{}` and {unsupported:?} is not implemented",
                ty.user_facing()
            )));
        }
    };
    Ok(Some(action))
}

pub(crate) fn destroy_action_for_type(
    ty: &ResolvedTy,
    ids: &PhysicalGlueIds,
) -> Option<DestroyAction> {
    match ty {
        _ if ids.resources.contains_key(ty) => Some(DestroyAction::Resource(ids.resources[ty])),
        _ if encoding_format(ty).is_some() => encoding_format(ty).map(DestroyAction::Encoding),
        ResolvedTy::Function { .. } | ResolvedTy::Closure { .. } => Some(DestroyAction::Callable),
        ResolvedTy::TraitObject { .. } => Some(DestroyAction::TraitObject),
        ResolvedTy::String => Some(DestroyAction::StringRelease),
        ResolvedTy::Bytes => Some(DestroyAction::BytesRelease),
        _ if ty.is_builtin(BuiltinType::Rc) => {
            ids.shared.get(ty).copied().map(DestroyAction::RcRelease)
        }
        _ if ty.is_builtin(BuiltinType::Weak) => Some(DestroyAction::WeakRelease),
        ResolvedTy::Array(_, _) if ids.vectors.contains_key(ty) => {
            Some(DestroyAction::Array(ids.vectors[ty]))
        }
        _ if ids.vectors.contains_key(ty) => Some(DestroyAction::Vector(ids.vectors[ty])),
        _ if ids.maps.contains_key(ty) => Some(DestroyAction::Map(ids.maps[ty])),
        _ if ids.sets.contains_key(ty) => Some(DestroyAction::Set(ids.sets[ty])),
        _ if ids.aggregates.contains_key(ty) => Some(DestroyAction::Aggregate(ids.aggregates[ty])),
        _ => ids.variants.get(ty).copied().map(DestroyAction::Variant),
    }
}

/// Resolve an encoding carrier through the shared canonical receiver contract.
#[must_use]
pub fn encoding_format(ty: &ResolvedTy) -> Option<EncodingFormat> {
    [EncodingFormat::Json, EncodingFormat::Yaml]
        .into_iter()
        .find(|format| {
            hew_types::RuntimeValueKind::Receiver(format.builtin())
                .resolve(Some(ty))
                .is_some()
        })
}

/// Collect the concrete semantic types that the physical module must realize.
///
/// Generic templates and unrelated checker fact rows are deliberately absent:
/// the inventory follows only callable headers and storage-producing types in
/// the verified concrete SIR module.
#[must_use]
pub fn physical_type_inventory(module: &SemModule) -> PhysicalTypeInventory {
    let mut types = BTreeSet::new();
    for callable in &module.callables {
        types.extend(
            callable
                .signature
                .params
                .iter()
                .map(|parameter| parameter.ty.clone()),
        );
        types.insert(callable.signature.return_ty.clone());
    }
    for function in &module.functions {
        types.insert(function.return_ty.clone());
        types.extend(function.params.iter().map(|parameter| parameter.ty.clone()));
        types.extend(function.places.iter().map(|place| place.ty.clone()));
        for block in &function.blocks {
            types.extend(block.args.iter().map(|argument| argument.ty.clone()));
            for operation in &block.ops {
                types.extend(operation.results.iter().map(|result| result.ty.clone()));
            }
            if let Some(result) = terminator_result(&block.terminator) {
                types.insert(result.ty.clone());
            }
            if let SemTerminator::WireCodec { plan, .. } = &block.terminator {
                plan.visit_types(&mut |ty| {
                    types.insert(ty.clone());
                });
            }
        }
    }
    for vtable in &module.vtables {
        types.insert(vtable.dyn_ty.clone());
        types.insert(vtable.concrete_ty.clone());
    }
    let mut inventory = PhysicalTypeInventory {
        types,
        resources: BTreeMap::new(),
        aggregates: BTreeMap::new(),
        variants: BTreeMap::new(),
        vectors: BTreeMap::new(),
        maps: BTreeMap::new(),
        sets: BTreeMap::new(),
        shared: BTreeMap::new(),
    };
    let demanded = inventory.types.iter().cloned().collect::<Vec<_>>();
    for ty in demanded {
        collect_inventory_type(module, &mut inventory, &ty);
    }
    inventory
}

pub(crate) fn collect_resource_type(
    module: &SemModule,
    inventory: &mut PhysicalTypeInventory,
    ty: &ResolvedTy,
) -> bool {
    let Some(release) = module.resources.get(ty) else {
        return false;
    };
    let Some(facts) = module.type_facts.get(&TypeInstanceKey(ty.clone())) else {
        return false;
    };
    if hew_sir::verify_resource_release(ty, release, facts).is_err() {
        return false;
    }
    inventory.resources.insert(
        ty.clone(),
        PhysicalResourceDescriptor {
            ty: ty.clone(),
            release: release.clone(),
        },
    );
    true
}

#[allow(
    clippy::too_many_lines,
    reason = "one recursive inventory walk keeps concrete type descriptors at the same boundary"
)]
pub(crate) fn collect_inventory_type(
    module: &SemModule,
    inventory: &mut PhysicalTypeInventory,
    ty: &ResolvedTy,
) {
    if inventory.resources.contains_key(ty)
        || inventory.aggregates.contains_key(ty)
        || inventory.variants.contains_key(ty)
        || inventory.vectors.contains_key(ty)
        || inventory.maps.contains_key(ty)
        || inventory.sets.contains_key(ty)
        || inventory.shared.contains_key(ty)
    {
        return;
    }
    inventory.types.insert(ty.clone());
    if collect_resource_type(module, inventory, ty) {
        if let Some((yielded, returned)) = hew_sir::generator_parts(ty) {
            collect_inventory_type(module, inventory, yielded);
            collect_inventory_type(module, inventory, returned);
        }
        // A record resource is released by its own `close`, and inside that
        // body its members drop directly, so it needs the aggregate glue as
        // well as the release. Every other resource is a bare handle.
        if !matches!(
            module.resources.get(ty),
            Some(hew_sir::ResourceRelease::RecordClose { .. })
        ) {
            return;
        }
    }
    if let ResolvedTy::Closure { captures, .. } = ty {
        for capture in captures {
            collect_inventory_type(module, inventory, capture);
        }
        return;
    }
    if let Some(element) = sequence_element_type(ty) {
        inventory.vectors.insert(
            ty.clone(),
            PhysicalVectorDescriptor {
                ty: ty.clone(),
                element: element.clone(),
            },
        );
        collect_inventory_type(module, inventory, element);
        return;
    }
    if let Some(payload) = shared_handle_payload(ty) {
        inventory.shared.insert(
            ty.clone(),
            PhysicalSharedDescriptor {
                ty: ty.clone(),
                payload: payload.clone(),
            },
        );
        collect_inventory_type(module, inventory, payload);
        return;
    }
    match collection_type_arguments(ty) {
        Some((BuiltinType::HashMap, args)) => {
            inventory.maps.insert(
                ty.clone(),
                PhysicalMapDescriptor {
                    ty: ty.clone(),
                    key: args[0].clone(),
                    value: args[1].clone(),
                },
            );
            for argument in args {
                collect_inventory_type(module, inventory, argument);
            }
            return;
        }
        Some((BuiltinType::HashSet, args)) => {
            inventory.sets.insert(
                ty.clone(),
                PhysicalSetDescriptor {
                    ty: ty.clone(),
                    element: args[0].clone(),
                },
            );
            collect_inventory_type(module, inventory, &args[0]);
            return;
        }
        _ => {}
    }
    if let Some(shape) = module.variant_shape_for_type(ty) {
        let variants = shape
            .variants
            .iter()
            .map(|variant| {
                variant
                    .fields
                    .iter()
                    .map(|field| field.ty.clone())
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();
        inventory.variants.insert(
            ty.clone(),
            PhysicalVariantDescriptor {
                ty: ty.clone(),
                is_indirect: shape.is_indirect,
                variants: variants.clone(),
            },
        );
        for fields in variants {
            for field in fields {
                collect_inventory_type(module, inventory, &field);
            }
        }
        return;
    }
    let fields = match ty {
        ResolvedTy::Tuple(fields) => Some(fields.clone()),
        _ => module
            .aggregate_shape_for_type(ty)
            .map(|shape| shape.fields.iter().map(|field| field.ty.clone()).collect()),
    };
    let Some(fields) = fields else {
        return;
    };
    inventory.aggregates.insert(
        ty.clone(),
        PhysicalAggregateDescriptor {
            ty: ty.clone(),
            fields: fields.clone(),
        },
    );
    for field in fields {
        collect_inventory_type(module, inventory, &field);
    }
}

/// The one rule that decides whether a parameter rides its own value or a
/// pointer to it. Direct calls, indirect invocations and vtable slots all
/// derive their ABI from this, so a dispatch and its implementation cannot
/// disagree about a carrier.
pub(crate) fn param_carrier(
    passing: hew_sir::SemParamPassing,
    layout: &PhysicalLayout,
) -> ParamCarrier {
    if passing == hew_sir::SemParamPassing::BorrowMut
        || matches!(layout.repr, PhysicalRepr::Struct(_))
    {
        ParamCarrier::Indirect
    } else {
        ParamCarrier::Direct
    }
}

pub(crate) fn required_layout<'a>(
    target: &'a PhysicalTarget,
    ty: &ResolvedTy,
) -> Result<&'a PhysicalLayout, PhysicalError> {
    target.layout(ty).ok_or_else(|| {
        PhysicalError::new(format!(
            "target `{}` has no concrete layout for `{}`",
            target.triple,
            ty.user_facing()
        ))
    })
}

pub(crate) struct FunctionLowerer<'a> {
    pub(crate) module: &'a SemModule,
    pub(crate) target: &'a PhysicalTarget,
    pub(crate) function: &'a SemFunction,
    pub(crate) glue_ids: &'a PhysicalGlueIds,
    /// Rendering recipes interned across the whole module, so two functions
    /// rendering the same type share one thunk.
    pub(crate) structural: &'a std::cell::RefCell<structural::StructuralGlue>,
    pub(crate) values: BTreeMap<ValueId, StorageId>,
    pub(crate) places: BTreeMap<hew_sir::PlaceId, StorageId>,
    pub(crate) storage: Vec<PhysicalStorage>,
    pub(crate) projections: &'a hew_sir::PlacePlan,
    pub(crate) lifetimes: &'a hew_sir::PlaceLifetimes,
}

#[allow(
    clippy::too_many_lines,
    reason = "one lowering pass allocates every verified SIR value, place, and variant-arm payload before lowering CFG bodies"
)]
pub(crate) fn lower_function(
    module: &SemModule,
    target: &PhysicalTarget,
    function: &SemFunction,
    glue_ids: &PhysicalGlueIds,
    structural: &std::cell::RefCell<structural::StructuralGlue>,
    certificate: &hew_sir::CheckedFunction,
) -> Result<(PhysicalFunction, Option<PhysicalDebugFunction>), PhysicalError> {
    let mut lowerer = FunctionLowerer {
        module,
        target,
        function,
        glue_ids,
        structural,
        values: BTreeMap::new(),
        places: BTreeMap::new(),
        storage: Vec::new(),
        projections: certificate.place_plan(),
        lifetimes: certificate.place_lifetimes(),
    };
    let mut parameters = Vec::with_capacity(function.params.len());
    for parameter in &function.params {
        parameters.push(lowerer.insert_value(
            parameter.value,
            &parameter.ty,
            parameter.own,
            StorageOrigin::Parameter(parameter.value),
        )?);
    }
    for block in &function.blocks {
        for argument in &block.args {
            lowerer.insert_value(
                argument.value,
                &argument.ty,
                argument.own,
                StorageOrigin::BlockArgument(argument.value),
            )?;
        }
        for operation in &block.ops {
            for result in &operation.results {
                lowerer.insert_value(
                    result.id,
                    &result.ty,
                    result.own,
                    StorageOrigin::Value(result.id),
                )?;
            }
        }
        if let Some(result) = terminator_result(&block.terminator) {
            lowerer.insert_value(
                result.id,
                &result.ty,
                result.own,
                StorageOrigin::Value(result.id),
            )?;
        }
        if let SemTerminator::Call {
            handback: Some(handback),
            ..
        } = &block.terminator
        {
            lowerer.insert_value(
                handback.id,
                &handback.ty,
                handback.own,
                StorageOrigin::Value(handback.id),
            )?;
        }
        if let SemTerminator::SwitchVariant { arms, .. } = &block.terminator {
            for arm in arms {
                for field in &arm.fields {
                    lowerer.insert_value(
                        field.id,
                        &field.ty,
                        field.own,
                        StorageOrigin::Value(field.id),
                    )?;
                }
            }
        }
    }
    for place in &function.places {
        let id = lowerer.next_storage_id()?;
        let previous = lowerer.places.insert(place.id, id);
        if previous.is_some() {
            return Err(PhysicalError::new(format!(
                "function `{}` declares physical place {} more than once",
                function.name, place.id.0
            )));
        }
        lowerer.storage.push(PhysicalStorage {
            id,
            ty: place.ty.clone(),
            layout: required_layout(target, &place.ty)?.clone(),
            own: if let Some(projection) = lowerer.projections.projection(place.id) {
                projection.recipe.own
            } else {
                OwnKind::of_ty(&place.ty, &module.type_facts).map_err(PhysicalError::new)?
            },
            origin: match place.origin {
                hew_sir::PlaceOrigin::ActorState {
                    state,
                    field,
                    initialized,
                    ..
                } => StorageOrigin::ActorState {
                    state: lowerer.value(state)?,
                    field,
                    initialized,
                },
                hew_sir::PlaceOrigin::Capture { environment, field } => StorageOrigin::Capture {
                    environment: lowerer.value(environment)?,
                    field,
                },
                hew_sir::PlaceOrigin::Aggregate { .. } => StorageOrigin::Aggregate(place.id),
                hew_sir::PlaceOrigin::Local => StorageOrigin::Local(place.id),
                hew_sir::PlaceOrigin::Runtime => {
                    return Err(PhysicalError::new(
                        "runtime place lacks a physical storage contract",
                    ));
                }
            },
            borrow_parent: None,
        });
    }

    for operation in function.blocks.iter().flat_map(|block| &block.ops) {
        if let Some(parent) = operation.kind.borrow_parent() {
            let dest = lowerer.one_result(operation)?;
            let source = match parent {
                hew_sir::PlaceBase::Place(place) => lowerer.place(place)?,
                hew_sir::PlaceBase::Value(value) => lowerer.value(value)?,
            };
            lowerer.storage[dest.0 as usize].borrow_parent = Some(source);
        }
    }

    // A runtime family whose contract result is a loan names argument zero as
    // the owner the result depends on. The dependency travels to the normal
    // successor's parameter, which is the loan the body reads.
    for block in &function.blocks {
        let hew_sir::SemTerminator::RtCall {
            family,
            args,
            result: hew_sir::CallResult::Value(value),
            normal,
            ..
        } = &block.terminator
        else {
            continue;
        };
        if !matches!(
            family.semantic_contract().map(|contract| contract.result),
            Some(hew_types::RuntimeResultEffect::Borrowed(_))
        ) {
            continue;
        }
        let owner = args
            .first()
            .ok_or_else(|| PhysicalError::new("borrowed runtime read has no receiver"))?;
        let source = lowerer.value(owner.operand.value)?;
        let dest = lowerer.value(value.id)?;
        // SIR decided whether this borrowed read is an actual loan: a result
        // with no ownership obligation was bit-copied out of the slot and
        // depends on no owner.
        if lowerer.storage[dest.0 as usize].own == OwnKind::Guaranteed {
            lowerer.storage[dest.0 as usize].borrow_parent = Some(source);
        }
        for parameter in blocks_by_id(function, normal.target)
            .map(|target| target.args.iter().map(|arg| arg.value).collect::<Vec<_>>())
            .unwrap_or_default()
        {
            let parameter = lowerer.value(parameter)?;
            if lowerer.storage[parameter.0 as usize].own == OwnKind::Guaranteed {
                lowerer.storage[parameter.0 as usize].borrow_parent = Some(source);
            }
        }
    }

    // A switch over a loaned scrutinee names it as the owner its payloads
    // depend on: the arm's parameters are loans of the same region.
    for block in &function.blocks {
        let hew_sir::SemTerminator::SwitchVariant {
            scrutinee, arms, ..
        } = &block.terminator
        else {
            continue;
        };
        let source = lowerer.value(scrutinee.value)?;
        if lowerer.storage[source.0 as usize].own != OwnKind::Guaranteed {
            continue;
        }
        for arm in arms {
            for field in &arm.fields {
                let dest = lowerer.value(field.id)?;
                if lowerer.storage[dest.0 as usize].own == OwnKind::Guaranteed {
                    lowerer.storage[dest.0 as usize].borrow_parent = Some(source);
                }
            }
            for parameter in blocks_by_id(function, arm.target.target)
                .map(|target| target.args.iter().map(|arg| arg.value).collect::<Vec<_>>())
                .unwrap_or_default()
            {
                let parameter = lowerer.value(parameter)?;
                if lowerer.storage[parameter.0 as usize].own == OwnKind::Guaranteed {
                    lowerer.storage[parameter.0 as usize].borrow_parent = Some(source);
                }
            }
        }
    }

    let cfg = hew_sir::build_cfg_index(function);
    let mut sites: BTreeMap<(BlockId, u32), u32> = BTreeMap::new();
    let blocks = function
        .blocks
        .iter()
        .filter(|block| cfg.reachable().contains(&block.id))
        .map(|block| {
            if !lowerer.lifetimes.is_reachable(block.id) {
                // Fault dispatch can prove one structurally present edge
                // impossible. Preserve its target identity without inventing
                // ownership certificates for code that cannot execute.
                return Ok(PhysicalBlock {
                    id: block.id,
                    arguments: block
                        .args
                        .iter()
                        .map(|arg| lowerer.value(arg.value))
                        .collect::<Result<_, _>>()?,
                    ops: vec![],
                    terminator: PhysicalTerminator::Unreachable,
                });
            }
            let arguments = block
                .args
                .iter()
                .map(|argument| lowerer.value(argument.value))
                .collect::<Result<Vec<_>, _>>()?;
            let ops = block
                .ops
                .iter()
                .try_fold(Vec::new(), |mut ops, operation| {
                    let first = ops.len();
                    ops.extend(lowerer.lower_op(operation, (block.id, ops.len()))?);
                    if let Some(offset) = module.debug.site_offset(&operation.provenance) {
                        for index in first..ops.len() {
                            sites.insert((block.id, index_key(index)), offset);
                        }
                    }
                    Ok::<_, PhysicalError>(ops)
                })?;
            if let Some(offset) = module.debug.site_offset(&block.terminator_provenance) {
                // The terminator's attribution takes the index one past the
                // last op, the position codegen reaches it at.
                sites.insert((block.id, index_key(ops.len())), offset);
            }
            let terminator = lowerer.lower_terminator(&block.terminator)?;
            Ok(PhysicalBlock {
                id: block.id,
                arguments,
                ops,
                terminator,
            })
        })
        .collect::<Result<Vec<_>, PhysicalError>>()?;

    let place_storage = lowerer.lower_place_storage()?;
    let attribution = function_attribution(function, &lowerer.storage, &parameters, sites);
    Ok((
        PhysicalFunction {
            callable: function.callable,
            entry: function.entry,
            parameters,
            place_storage,
            storage: lowerer.storage,
            blocks,
        },
        attribution,
    ))
}

/// An operation's position within its block, as debug attribution keys it.
/// A block with more than `u32::MAX` operations cannot exist.
pub(crate) fn index_key(index: usize) -> u32 {
    u32::try_from(index).unwrap_or(u32::MAX)
}

/// Join SIR's ordered binding table onto the storage that realizes each source
/// name. Only a root-unit body is attributed: another module's spans index a
/// different file.
pub(crate) fn function_attribution(
    function: &SemFunction,
    storage: &[PhysicalStorage],
    parameters: &[StorageId],
    sites: BTreeMap<(BlockId, u32), u32>,
) -> Option<PhysicalDebugFunction> {
    if function.source_origin != hew_sir::FunctionSourceOrigin::RootUnit {
        return None;
    }
    let mut locals = BTreeMap::new();
    for entry in storage {
        let binding = match entry.origin {
            StorageOrigin::Local(place) | StorageOrigin::Aggregate(place) => {
                function.binding_rooting(place)
            }
            StorageOrigin::Parameter(value)
            | StorageOrigin::Value(value)
            | StorageOrigin::BlockArgument(value) => function.binding_naming(value),
            StorageOrigin::ActorState { .. } | StorageOrigin::Capture { .. } => None,
        };
        let Some(binding) = binding else { continue };
        locals.insert(
            entry.id,
            PhysicalDebugLocal {
                name: binding.name.clone(),
                decl: u32::try_from(binding.span.start).unwrap_or(u32::MAX),
                parameter: parameters
                    .iter()
                    .position(|parameter| *parameter == entry.id)
                    .and_then(|index| u32::try_from(index + 1).ok()),
            },
        );
    }
    Some(PhysicalDebugFunction {
        name: function.name.clone(),
        decl: u32::try_from(function.span.start).unwrap_or(u32::MAX),
        end: u32::try_from(function.span.end).unwrap_or(u32::MAX),
        locals,
        sites,
    })
}

pub(crate) fn terminator_result(terminator: &SemTerminator) -> Option<&hew_sir::ValueDef> {
    match terminator {
        SemTerminator::Call {
            result: CallResult::Value(result),
            ..
        }
        | SemTerminator::WireCodec {
            result: CallResult::Value(result),
            ..
        }
        | SemTerminator::RtCall {
            result: CallResult::Value(result),
            ..
        }
        | SemTerminator::ExternCall {
            result: CallResult::Value(result),
            ..
        }
        | SemTerminator::ActorCall {
            result: CallResult::Value(result),
            ..
        }
        | SemTerminator::IndirectCall {
            result: CallResult::Value(result),
            ..
        }
        | SemTerminator::DynCall {
            result: CallResult::Value(result),
            ..
        }
        | SemTerminator::ValueCall {
            result: CallResult::Value(result),
            ..
        }
        | SemTerminator::Suspend {
            result: CallResult::Value(result),
            ..
        }
        | SemTerminator::CheckedBinary { result, .. }
        | SemTerminator::RecoverFault { result, .. } => Some(result),
        _ => None,
    }
}

pub(crate) fn blocks_by_id(
    function: &hew_sir::SemFunction,
    id: hew_sir::BlockId,
) -> Option<&hew_sir::SemBlock> {
    function.blocks.iter().find(|block| block.id == id)
}
