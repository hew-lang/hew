//! Independent verification of physical structure: resources, structural glue, clone/destroy actions and aggregate/variant/tuple construction.

use super::{
    callable, capability, collection_type_arguments, encoding_format, partial, required_layout,
    sequence_element_type, suspend, verify_initialization, verify_operation_storage,
    verify_terminator, wire, BTreeSet, BuiltinType, CloneAction, CloneKind, DestroyAction, OwnKind,
    ParamCarrier, PhysicalAggregateGlue, PhysicalAggregateId, PhysicalError, PhysicalFunction,
    PhysicalLayout, PhysicalMapGlue, PhysicalMapId, PhysicalModule, PhysicalRepr, PhysicalSetGlue,
    PhysicalSetId, PhysicalSharedGlue, PhysicalSharedId, PhysicalStorage, PhysicalStructuralId,
    PhysicalStructuralShape, PhysicalValueRecipe, PhysicalVariantCase, PhysicalVariantGlue,
    PhysicalVariantId, PhysicalVectorGlue, PhysicalVectorId, ResolvedTy, SemParamPassing,
    StorageId, StorageOrigin, TypeInstanceKey,
};

pub(crate) fn verify_resources(module: &PhysicalModule) -> Result<(), PhysicalError> {
    for recipe in module.actor_recipes.values() {
        verify_value_recipe(module, recipe)?;
    }
    let mut resource_types = BTreeSet::new();
    for resource in &module.resources {
        if !resource_types.insert(&resource.ty) {
            return Err(PhysicalError::new("duplicate physical resource authority"));
        }
        hew_sir::verify_resource_release(
            &resource.ty,
            &resource.release,
            semantic_type_facts(module, &resource.ty)?,
        )
        .map_err(PhysicalError::new)?;
        // A release that runs an authored `close` names the exact callable
        // that executes it; the backend calls that one and derives nothing.
        let authored_close = match &resource.release {
            hew_sir::ResourceRelease::RecordClose { lifecycle, close } => {
                Some((&lifecycle.close_declaration, *close))
            }
            hew_sir::ResourceRelease::OpaqueClose { lifecycle, close } => {
                Some((&lifecycle.close_declaration, *close))
            }
            _ => None,
        };
        if let Some((declaration, close)) = authored_close {
            let callable = module
                .callables
                .get(close.0 as usize)
                .ok_or_else(|| PhysicalError::new("authored release names no admitted callable"))?;
            if callable.declaration != *declaration
                || callable.params.len() != 1
                || callable.params[0].ty != resource.ty
                || callable.return_ty != ResolvedTy::Unit
            {
                return Err(PhysicalError::new(
                    "authored release callable does not consume one exact owner and return unit",
                ));
            }
        }
        let expected = match resource.release.carrier().map_err(PhysicalError::new)? {
            hew_sir::ResourceCarrier::Pointer => PhysicalRepr::Pointer,
            hew_sir::ResourceCarrier::I32 => PhysicalRepr::Integer { bits: 32 },
            hew_sir::ResourceCarrier::Record => {
                if !matches!(
                    required_layout(&module.target, &resource.ty)?.repr,
                    PhysicalRepr::Struct(_)
                ) {
                    return Err(PhysicalError::new(
                        "record release requires its exact field-bearing layout",
                    ));
                }
                continue;
            }
        };
        if required_layout(&module.target, &resource.ty)?.repr != expected {
            return Err(PhysicalError::new(
                "resource release requires its exact checked carrier",
            ));
        }
    }
    Ok(())
}

/// Every rendering recipe sits at its own identity, has a realized layout and
/// names only recipes this table carries. Codegen emits one thunk per row, so
/// a dangling member reference would emit a call to nothing.
pub(crate) fn verify_structural_glue(module: &PhysicalModule) -> Result<(), PhysicalError> {
    let count = module.structural_glue.len();
    for (index, glue) in module.structural_glue.iter().enumerate() {
        if usize::try_from(glue.id.0).ok() != Some(index) {
            return Err(PhysicalError::new(format!(
                "structural recipe {} is not at its canonical table index {index}",
                glue.id.0
            )));
        }
        required_layout(&module.target, &glue.ty)?;
        let members: Vec<PhysicalStructuralId> = match &glue.shape {
            PhysicalStructuralShape::Tuple { fields } => fields.clone(),
            PhysicalStructuralShape::Record { fields, .. } => {
                fields.iter().map(|field| field.recipe).collect()
            }
            PhysicalStructuralShape::Enum { cases } => cases
                .iter()
                .flat_map(|case| case.fields.iter().map(|field| field.recipe))
                .collect(),
            PhysicalStructuralShape::Display { callable } => {
                let callee = module
                    .callables
                    .get(callable.0 as usize)
                    .filter(|callee| callee.id == *callable)
                    .ok_or_else(|| {
                        PhysicalError::new("structural Display names an absent callable")
                    })?;
                if callee.params.len() != 1
                    || callee.params[0].ty != glue.ty
                    || !matches!(
                        callee.params[0].passing,
                        SemParamPassing::Borrow | SemParamPassing::ReadOnly
                    )
                    || callee.return_ty != ResolvedTy::String
                {
                    return Err(PhysicalError::new(
                        "structural Display has an incompatible borrowed formatter signature",
                    ));
                }
                Vec::new()
            }
            PhysicalStructuralShape::Vector { element } => vec![*element],
            PhysicalStructuralShape::Map { key, value } => vec![*key, *value],
            PhysicalStructuralShape::SignedInt
            | PhysicalStructuralShape::UnsignedInt
            | PhysicalStructuralShape::Float
            | PhysicalStructuralShape::Bool
            | PhysicalStructuralShape::Char
            | PhysicalStructuralShape::Unit
            | PhysicalStructuralShape::String
            | PhysicalStructuralShape::Identity { .. } => Vec::new(),
        };
        for member in members {
            if usize::try_from(member.0).ok().is_none_or(|id| id >= count) {
                return Err(PhysicalError::new(format!(
                    "structural recipe {} names recipe {}, which the module does not carry",
                    glue.id.0, member.0
                )));
            }
        }
    }
    Ok(())
}

pub(crate) fn verify_physical_module(module: &PhysicalModule) -> Result<(), PhysicalError> {
    capability::verify(module)?;
    wire::verify_actor_codecs(module)?;
    verify_resources(module)?;
    if module.target.triple.is_empty() || module.target.data_layout.is_empty() {
        return Err(PhysicalError::new(
            "physical module requires a target triple and data layout",
        ));
    }
    verify_structural_glue(module)?;
    for (index, glue) in module.aggregate_glue.iter().enumerate() {
        if usize::try_from(glue.id.0).ok() != Some(index) {
            return Err(PhysicalError::new(format!(
                "physical aggregate glue {} is not at its canonical table index {index}",
                glue.id.0
            )));
        }
        if glue.own == OwnKind::Guaranteed {
            return Err(PhysicalError::new(format!(
                "physical aggregate glue {} carries a borrow-only ownership class",
                glue.id.0
            )));
        }
        let layout = required_layout(&module.target, &glue.ty)?;
        let PhysicalRepr::Struct(layout_fields) = &layout.repr else {
            return Err(PhysicalError::new(format!(
                "physical aggregate glue {} has a non-aggregate layout",
                glue.id.0
            )));
        };
        if layout_fields.len() != glue.fields.len() {
            return Err(PhysicalError::new(format!(
                "physical aggregate glue {} has {} recipes for {} layout fields",
                glue.id.0,
                glue.fields.len(),
                layout_fields.len()
            )));
        }
        for (field_index, (field, layout_field)) in
            glue.fields.iter().zip(layout_fields).enumerate()
        {
            if field.own == OwnKind::Guaranteed {
                return Err(PhysicalError::new(format!(
                    "physical aggregate glue {} field {field_index} carries a borrow-only obligation",
                    glue.id.0
                )));
            }
            if module.target.layout(&field.ty) != Some(layout_field) {
                return Err(PhysicalError::new(format!(
                    "physical aggregate glue {} field {field_index} layout disagrees with target authority",
                    glue.id.0
                )));
            }
            verify_value_recipe(module, field)?;
        }
    }
    for (index, glue) in module.variant_glue.iter().enumerate() {
        verify_variant_glue(module, index, glue)?;
    }
    verify_collection_glue_tables(module)?;
    for (index, callable) in module.callables.iter().enumerate() {
        if usize::try_from(callable.id.0).ok() != Some(index) {
            return Err(PhysicalError::new(format!(
                "physical callable {} is not at its canonical table index {index}",
                callable.id.0
            )));
        }
    }
    if let Some(entry) = module.entry_callable {
        if module
            .callables
            .get(entry.0 as usize)
            .is_none_or(|callable| callable.id != entry)
        {
            return Err(PhysicalError::new(format!(
                "physical entry callable {} is absent from the callable table",
                entry.0
            )));
        }
    }
    let mut function_ids = BTreeSet::new();
    for function in &module.functions {
        if !function_ids.insert(function.callable) {
            return Err(PhysicalError::new(format!(
                "physical callable {} has more than one body",
                function.callable.0
            )));
        }
        verify_physical_function(module, function)?;
    }
    suspend::verify_callables(module)?;
    Ok(())
}

pub(crate) fn verify_environment_glue(module: &PhysicalModule) -> Result<(), PhysicalError> {
    callable::verify_closures(module)?;
    let mut environment_types = BTreeSet::new();
    for glue in &module.environment_glue {
        if !environment_types.insert(&glue.ty) {
            return Err(PhysicalError::new("duplicate physical environment type"));
        }
        let captures = match &glue.ty {
            ResolvedTy::Closure { captures, .. } => captures.as_slice(),
            ResolvedTy::Function { .. } => &[],
            _ => {
                return Err(PhysicalError::new(
                    "environment recipe has no callable type",
                ))
            }
        };
        let facts = semantic_type_facts(module, &glue.ty)?;
        if !matches!(
            facts.clone,
            CloneKind::None | CloneKind::DeepCopy | CloneKind::FieldWise
        ) || glue.cloneable != (facts.clone != CloneKind::None)
            || captures.len() != glue.fields.len()
        {
            return Err(PhysicalError::new(
                "environment recipe differs from concrete callable facts",
            ));
        }
        let layout = module
            .target
            .environment_layout(&glue.ty)
            .ok_or_else(|| PhysicalError::new("environment recipe lacks a target layout"))?;
        if captures.is_empty() {
            if layout.size != 0 || layout.align != 1 {
                return Err(PhysicalError::new("empty callable environment has storage"));
            }
        } else {
            let PhysicalRepr::Struct(fields) = &layout.repr else {
                return Err(PhysicalError::new(
                    "captured environment lacks its mask and field struct",
                ));
            };
            if fields.len() != captures.len() + 1 {
                return Err(PhysicalError::new("environment field layout count differs"));
            }
            let PhysicalRepr::Array { element, len } = &fields[0].repr else {
                return Err(PhysicalError::new("environment mask is not a byte array"));
            };
            if element.repr != (PhysicalRepr::Integer { bits: 8 })
                || usize::try_from(*len).ok() != Some(captures.len().div_ceil(8))
                || fields[0].size != u64::from(*len)
            {
                return Err(PhysicalError::new(
                    "environment mask differs from logical capture count",
                ));
            }
            for (ty, layout) in captures.iter().zip(&fields[1..]) {
                if Some(layout) != module.target.layout(ty) {
                    return Err(PhysicalError::new(
                        "environment capture layout differs from target type",
                    ));
                }
            }
        }
        for (capture, recipe) in captures.iter().zip(&glue.fields) {
            if capture != &recipe.ty || (glue.cloneable && recipe.clone.is_none()) {
                return Err(PhysicalError::new(
                    "environment field lacks its exact copy contract",
                ));
            }
            verify_value_recipe(module, recipe)?;
        }
    }
    Ok(())
}

pub(crate) fn verify_collection_glue_tables(module: &PhysicalModule) -> Result<(), PhysicalError> {
    verify_environment_glue(module)?;
    let mut vector_types = BTreeSet::new();
    for (index, glue) in module.vector_glue.iter().enumerate() {
        if !vector_types.insert(&glue.ty) {
            return Err(PhysicalError::new(
                "physical vector type has more than one glue identity",
            ));
        }
        verify_vector_glue(module, index, glue)?;
    }
    let mut map_types = BTreeSet::new();
    for (index, glue) in module.map_glue.iter().enumerate() {
        if usize::try_from(glue.id.0).ok() != Some(index) || !map_types.insert(&glue.ty) {
            return Err(PhysicalError::new(
                "physical map glue has a noncanonical identity",
            ));
        }
        verify_collection_value_glue(
            module,
            &glue.ty,
            BuiltinType::HashMap,
            &[&glue.key, &glue.value],
        )?;
    }
    let mut set_types = BTreeSet::new();
    for (index, glue) in module.set_glue.iter().enumerate() {
        if usize::try_from(glue.id.0).ok() != Some(index) || !set_types.insert(&glue.ty) {
            return Err(PhysicalError::new(
                "physical set glue has a noncanonical identity",
            ));
        }
        verify_collection_value_glue(module, &glue.ty, BuiltinType::HashSet, &[&glue.element])?;
    }
    Ok(())
}

pub(crate) fn semantic_type_facts<'a>(
    module: &'a PhysicalModule,
    ty: &ResolvedTy,
) -> Result<&'a hew_types::TypeFacts, PhysicalError> {
    module
        .type_facts
        .get(&TypeInstanceKey(ty.clone()))
        .ok_or_else(|| {
            PhysicalError::new(format!(
                "physical value `{}` has no retained semantic type facts",
                ty.user_facing()
            ))
        })
}

pub(crate) fn verify_value_recipe(
    module: &PhysicalModule,
    field: &PhysicalValueRecipe,
) -> Result<(), PhysicalError> {
    let facts = semantic_type_facts(module, &field.ty)?;
    if field.own != OwnKind::of_class(facts.class)
        || field.clone.is_some() != (facts.clone != CloneKind::None)
        || field.destroy.is_some() != (field.own == OwnKind::Owned)
    {
        return Err(PhysicalError::new(format!(
            "physical field recipe for `{}` disagrees with semantic ownership or cloneability",
            field.ty.user_facing()
        )));
    }
    if let Some(action) = field.clone {
        verify_clone_action(module, &field.ty, field.own, action)?;
    }
    if let Some(action) = field.destroy {
        verify_destroy_action(module, &field.ty, field.own, action)?;
    }
    Ok(())
}

/// Check fixed-array allocation geometry against the target pointer width and
/// the runtime's signed length ABI. This is shared by target realization and
/// physical verification; source size is never inferred from a stack budget.
///
/// # Errors
/// Refuses an invalid element layout, overflowing allocation or unrepresentable length.
pub fn validate_array_allocation(
    length: u64,
    element: &PhysicalLayout,
    pointer_bytes: u64,
) -> Result<(), PhysicalError> {
    if !(1..=8).contains(&pointer_bytes)
        || element.align == 0
        || !element.align.is_power_of_two()
        || element.size > i64::MAX as u64
        || !element.size.is_multiple_of(u64::from(element.align))
    {
        return Err(PhysicalError::new(
            "fixed array has an invalid target element layout",
        ));
    }
    let bits = pointer_bytes * 8;
    let length_limit = if bits == 64 {
        i64::MAX as u64
    } else {
        (1u64 << bits) - 1
    };
    let allocation_limit = (1u64 << (bits - 1)) - 1;
    let padded_limit = allocation_limit.checked_sub(u64::from(element.align - 1));
    let bytes = length.checked_mul(element.size).filter(|bytes| {
        let allocation_bytes = if length == 0 { 0 } else { (*bytes).max(1) };
        padded_limit.is_some_and(|limit| allocation_bytes <= limit)
    });
    if length > length_limit || bytes.is_none() {
        return Err(PhysicalError::new(
            "fixed array exceeds the target allocation or runtime length range",
        ));
    }
    Ok(())
}

pub(crate) fn verify_vector_glue(
    module: &PhysicalModule,
    index: usize,
    glue: &PhysicalVectorGlue,
) -> Result<(), PhysicalError> {
    if usize::try_from(glue.id.0).ok() != Some(index) {
        return Err(PhysicalError::new(format!(
            "physical vector glue {} is not at its canonical table index {index}",
            glue.id.0
        )));
    }
    if sequence_element_type(&glue.ty) != Some(&glue.element.ty) {
        return Err(PhysicalError::new(
            "physical sequence descriptor disagrees with its exact element identity",
        ));
    }
    if let ResolvedTy::Array(_, length) = &glue.ty {
        let element_layout = required_layout(&module.target, &glue.element.ty)?;
        let carrier = required_layout(&module.target, &glue.ty)?;
        validate_array_allocation(*length, element_layout, carrier.size)?;
    }
    let vector_facts = semantic_type_facts(module, &glue.ty)?;
    if OwnKind::of_class(vector_facts.class) != OwnKind::Owned {
        return Err(PhysicalError::new(
            "physical vector has no semantic owning contract",
        ));
    }
    if required_layout(&module.target, &glue.ty)?.repr != PhysicalRepr::Pointer {
        return Err(PhysicalError::new(
            "physical vector descriptor requires its target pointer carrier",
        ));
    }
    let element_layout = required_layout(&module.target, &glue.element.ty)?;
    if !element_layout.align.is_power_of_two() {
        return Err(PhysicalError::new(
            "physical vector element has an invalid target alignment",
        ));
    }
    // Zero-sized elements retain their exact target size. The runtime owns
    // allocation bookkeeping; no payload byte is invented here.
    verify_value_recipe(module, &glue.element)?;
    if vector_facts.clone != CloneKind::None && glue.element.clone.is_none() {
        return Err(PhysicalError::new(
            "physical vector element has no clone action",
        ));
    }
    Ok(())
}

pub(crate) fn verify_collection_value_glue(
    module: &PhysicalModule,
    ty: &ResolvedTy,
    kind: BuiltinType,
    recipes: &[&PhysicalValueRecipe],
) -> Result<(), PhysicalError> {
    let Some((actual, arguments)) = collection_type_arguments(ty) else {
        return Err(PhysicalError::new(
            "physical collection lacks a canonical type identity",
        ));
    };
    if actual != kind
        || arguments.len() != recipes.len()
        || !arguments
            .iter()
            .zip(recipes)
            .all(|(argument, recipe)| argument == &recipe.ty)
    {
        return Err(PhysicalError::new(
            "physical collection recipes disagree with type arguments",
        ));
    }
    let facts = semantic_type_facts(module, ty)?;
    // A map whose value has no clone has none itself; its own clone action is
    // required only where the semantics keep one.
    let clonable = matches!(facts.clone, CloneKind::DeepCopy | CloneKind::FieldWise);
    if OwnKind::of_class(facts.class) != OwnKind::Owned
        || (!clonable && facts.clone != CloneKind::None)
        || required_layout(&module.target, ty)?.repr != PhysicalRepr::Pointer
    {
        return Err(PhysicalError::new(
            "physical collection lacks an owning pointer copy contract",
        ));
    }
    for recipe in recipes {
        let layout = required_layout(&module.target, &recipe.ty)?;
        if !layout.align.is_power_of_two() || layout.size % u64::from(layout.align) != 0 {
            return Err(PhysicalError::new(
                "physical collection element has an invalid target layout",
            ));
        }
        verify_value_recipe(module, recipe)?;
        if clonable && recipe.clone.is_none() {
            return Err(PhysicalError::new(
                "physical collection element has no clone action",
            ));
        }
    }
    Ok(())
}

pub(crate) fn verify_variant_glue(
    module: &PhysicalModule,
    index: usize,
    glue: &PhysicalVariantGlue,
) -> Result<(), PhysicalError> {
    if usize::try_from(glue.id.0).ok() != Some(index) {
        return Err(PhysicalError::new(format!(
            "physical variant glue {} is not at its canonical table index {index}",
            glue.id.0
        )));
    }
    if glue.own == OwnKind::Guaranteed {
        return Err(PhysicalError::new(format!(
            "physical variant glue {} carries a borrow-only ownership class",
            glue.id.0
        )));
    }
    let layout = module.target.variant_layout(&glue.ty).ok_or_else(|| {
        PhysicalError::new(format!(
            "physical variant glue {} has no target variant layout",
            glue.id.0
        ))
    })?;
    let PhysicalRepr::Struct(object_fields) = &layout.object.repr else {
        return Err(PhysicalError::new(format!(
            "physical variant glue {} has a non-struct object layout",
            glue.id.0
        )));
    };
    let expected_tag_bits = match glue.variants.len() {
        0..=256 => 8,
        257..=65_536 => 16,
        count => {
            return Err(PhysicalError::new(format!(
                "physical variant glue {} has unsupported case count {count}",
                glue.id.0
            )));
        }
    };
    if object_fields.len() != 2
        || object_fields[0].repr
            != (PhysicalRepr::Integer {
                bits: expected_tag_bits,
            })
        || !matches!(object_fields[1].repr, PhysicalRepr::Array { .. })
    {
        return Err(PhysicalError::new(format!(
            "physical variant glue {} object layout lacks its exact tag and payload carriers",
            glue.id.0
        )));
    }
    // An indirect enum value is one pointer to its heap node; the node keeps
    // the tag-and-payload object layout. A direct enum value is that object.
    let value_matches = match module.target.layout(&glue.ty) {
        Some(value) if glue.is_indirect => value.repr == PhysicalRepr::Pointer,
        Some(value) => value == &layout.object,
        None => false,
    };
    if !value_matches
        || layout.is_indirect != glue.is_indirect
        || layout.variants.len() != glue.variants.len()
    {
        return Err(PhysicalError::new(format!(
            "physical variant glue {} disagrees with target variant shape",
            glue.id.0
        )));
    }
    let payload_carrier = &object_fields[1];
    for (variant_index, (variant, variant_layout)) in
        glue.variants.iter().zip(&layout.variants).enumerate()
    {
        if payload_carrier.size < variant_layout.size
            || payload_carrier.align < variant_layout.align
        {
            return Err(PhysicalError::new(format!(
                "physical variant glue {} payload carrier cannot hold case {variant_index}",
                glue.id.0
            )));
        }
        let PhysicalRepr::Struct(layout_fields) = &variant_layout.repr else {
            return Err(PhysicalError::new(format!(
                "physical variant glue {} case {variant_index} has a non-struct payload layout",
                glue.id.0
            )));
        };
        if layout_fields.len() != variant.fields.len() {
            return Err(PhysicalError::new(format!(
                    "physical variant glue {} case {variant_index} recipe count disagrees with its payload layout",
                    glue.id.0
                )));
        }
        for (field_index, (field, layout_field)) in
            variant.fields.iter().zip(layout_fields).enumerate()
        {
            if field.own == OwnKind::Guaranteed
                || module.target.layout(&field.ty) != Some(layout_field)
            {
                return Err(PhysicalError::new(format!(
                        "physical variant glue {} case {variant_index} field {field_index} disagrees with target authority",
                        glue.id.0
                    )));
            }
            verify_value_recipe(module, field)?;
        }
    }
    Ok(())
}

#[allow(
    clippy::too_many_lines,
    reason = "keep per-function ABI, storage and cleanup contracts together"
)]
pub(crate) fn verify_physical_function(
    module: &PhysicalModule,
    function: &PhysicalFunction,
) -> Result<(), PhysicalError> {
    suspend::verify_task_scopes(function)?;
    let callable = module
        .callables
        .get(function.callable.0 as usize)
        .filter(|candidate| candidate.id == function.callable)
        .ok_or_else(|| {
            PhysicalError::new(format!(
                "physical function has unknown callable {}",
                function.callable.0
            ))
        })?;
    if function.parameters.len() != callable.params.len() {
        return Err(PhysicalError::new(format!(
            "physical callable {} has {} parameter storage slots for {} ABI parameters",
            function.callable.0,
            function.parameters.len(),
            callable.params.len()
        )));
    }
    callable::verify_capture_slots(module, function)?;
    partial::verify_storage(module, function)?;
    for (index, storage) in function.storage.iter().enumerate() {
        if usize::try_from(storage.id.0).ok() != Some(index) {
            return Err(PhysicalError::new(format!(
                "physical storage {} is not at canonical index {index}",
                storage.id.0
            )));
        }
        if module.target.layout(&storage.ty) != Some(&storage.layout) {
            return Err(PhysicalError::new(format!(
                "physical storage {} layout disagrees with target authority",
                storage.id.0
            )));
        }
        if let Some(parent) = storage.borrow_parent {
            if storage.own != OwnKind::Guaranteed
                || parent == storage.id
                || function.storage.get(parent.0 as usize).is_none()
            {
                return Err(PhysicalError::new(
                    "physical loan storage has an invalid SIR parent dependency",
                ));
            }
        } else if storage.own == OwnKind::Guaranteed
            && !matches!(storage.origin, StorageOrigin::Parameter(_))
        {
            return Err(PhysicalError::new(
                "physical local loan storage has no SIR parent dependency",
            ));
        }
    }
    for (index, (parameter, abi)) in function.parameters.iter().zip(&callable.params).enumerate() {
        let slot = storage(function, *parameter)?;
        let expected_own = match abi.passing {
            hew_sir::SemParamPassing::ReadOnly => OwnKind::None,
            hew_sir::SemParamPassing::Borrow | hew_sir::SemParamPassing::BorrowMut => {
                OwnKind::Guaranteed
            }
            hew_sir::SemParamPassing::Consume => {
                OwnKind::of_param(&abi.ty, abi.passing, &module.type_facts)
                    .map_err(PhysicalError::new)?
            }
        };
        if abi.passing == hew_sir::SemParamPassing::BorrowMut
            && abi.carrier != ParamCarrier::Indirect
        {
            return Err(PhysicalError::new(
                "physical exclusive parameter requires caller storage by address",
            ));
        }
        if slot.ty != abi.ty || slot.own != expected_own {
            return Err(PhysicalError::new(format!(
                "physical callable {} parameter {index} disagrees with its ABI type or ownership",
                function.callable.0
            )));
        }
    }
    let block_ids = function
        .blocks
        .iter()
        .map(|block| block.id)
        .collect::<BTreeSet<_>>();
    if !block_ids.contains(&function.entry) {
        return Err(PhysicalError::new(format!(
            "physical function {} has no entry block {}",
            function.callable.0, function.entry.0
        )));
    }
    for block in &function.blocks {
        for operation in &block.ops {
            verify_operation_storage(module, function, operation)?;
        }
        verify_terminator(module, function, &block_ids, &block.terminator)?;
    }
    // Compute the suffix facts once, retaining any error. Initialization keeps
    // its existing diagnostic priority over stale physical cleanup sites.
    let needs_fault = partial::verify_trap_cleanup_refinement(function);
    verify_initialization(module, function, needs_fault.as_ref().ok())?;
    for block in &function.blocks {
        for (index, operation) in block.ops.iter().enumerate() {
            partial::verify_cleanup_site(function, operation, (block.id, index))?;
        }
    }
    needs_fault.map(|_| ())
}

pub(crate) fn storage(
    function: &PhysicalFunction,
    id: StorageId,
) -> Result<&PhysicalStorage, PhysicalError> {
    function
        .storage
        .get(id.0 as usize)
        .filter(|candidate| candidate.id == id)
        .ok_or_else(|| PhysicalError::new(format!("unknown physical storage {}", id.0)))
}

pub(crate) fn require_same_storage_type(
    function: &PhysicalFunction,
    left: StorageId,
    right: StorageId,
    context: &str,
) -> Result<(), PhysicalError> {
    if storage(function, left)?.ty == storage(function, right)?.ty {
        Ok(())
    } else {
        Err(PhysicalError::new(format!(
            "{context} uses incompatible physical storage types"
        )))
    }
}

pub(crate) fn aggregate_glue(
    module: &PhysicalModule,
    id: PhysicalAggregateId,
) -> Result<&PhysicalAggregateGlue, PhysicalError> {
    module
        .aggregate_glue
        .get(id.0 as usize)
        .filter(|glue| glue.id == id)
        .ok_or_else(|| PhysicalError::new(format!("unknown physical aggregate glue {}", id.0)))
}

pub(crate) fn variant_glue(
    module: &PhysicalModule,
    id: PhysicalVariantId,
) -> Result<&PhysicalVariantGlue, PhysicalError> {
    module
        .variant_glue
        .get(id.0 as usize)
        .filter(|glue| glue.id == id)
        .ok_or_else(|| PhysicalError::new(format!("unknown physical variant glue {}", id.0)))
}

pub(crate) fn vector_glue(
    module: &PhysicalModule,
    id: PhysicalVectorId,
) -> Result<&PhysicalVectorGlue, PhysicalError> {
    module
        .vector_glue
        .get(id.0 as usize)
        .filter(|glue| glue.id == id)
        .ok_or_else(|| PhysicalError::new(format!("unknown physical vector glue {}", id.0)))
}

pub(crate) fn map_glue(
    module: &PhysicalModule,
    id: PhysicalMapId,
) -> Result<&PhysicalMapGlue, PhysicalError> {
    module
        .map_glue
        .get(id.0 as usize)
        .filter(|glue| glue.id == id)
        .ok_or_else(|| PhysicalError::new(format!("unknown physical map glue {}", id.0)))
}

pub(crate) fn shared_glue(
    module: &PhysicalModule,
    id: PhysicalSharedId,
) -> Result<&PhysicalSharedGlue, PhysicalError> {
    module
        .shared_glue
        .get(id.0 as usize)
        .filter(|glue| glue.id == id)
        .ok_or_else(|| PhysicalError::new(format!("unknown physical shared glue {}", id.0)))
}

pub(crate) fn set_glue(
    module: &PhysicalModule,
    id: PhysicalSetId,
) -> Result<&PhysicalSetGlue, PhysicalError> {
    module
        .set_glue
        .get(id.0 as usize)
        .filter(|glue| glue.id == id)
        .ok_or_else(|| PhysicalError::new(format!("unknown physical set glue {}", id.0)))
}

pub(crate) fn verify_clone_action(
    module: &PhysicalModule,
    ty: &ResolvedTy,
    own: OwnKind,
    action: CloneAction,
) -> Result<(), PhysicalError> {
    let facts = semantic_type_facts(module, ty)?;
    let clone_kind_matches = matches!(
        (facts.clone, action),
        (CloneKind::Bits, CloneAction::Bitwise)
            | (CloneKind::DeepCopy, CloneAction::Encoding(_))
            | (
                CloneKind::Retain,
                CloneAction::StringRetain
                    | CloneAction::BytesRetain
                    | CloneAction::RcRetain
                    | CloneAction::WeakRetain
            )
            | (
                CloneKind::FieldWise,
                CloneAction::Aggregate(_) | CloneAction::Variant(_)
            )
            | (
                CloneKind::DeepCopy | CloneKind::FieldWise,
                CloneAction::Callable
                    | CloneAction::Vector(_)
                    | CloneAction::Array(_)
                    | CloneAction::Map(_)
                    | CloneAction::Set(_)
            )
    );
    let valid = clone_kind_matches
        && own == OwnKind::of_class(facts.class)
        && match action {
            CloneAction::Encoding(format) => {
                own == OwnKind::Owned && encoding_format(ty) == Some(format)
            }
            CloneAction::Callable => {
                own == OwnKind::Owned
                    && matches!(ty,
                    ResolvedTy::Function { capabilities, .. } | ResolvedTy::Closure { capabilities, .. }
                    if capabilities.clone)
            }
            CloneAction::Bitwise => own == OwnKind::None,
            CloneAction::StringRetain => ty == &ResolvedTy::String && own == OwnKind::Owned,
            CloneAction::BytesRetain => ty == &ResolvedTy::Bytes && own == OwnKind::Owned,
            CloneAction::RcRetain => ty.is_builtin(BuiltinType::Rc) && own == OwnKind::Owned,
            CloneAction::WeakRetain => ty.is_builtin(BuiltinType::Weak) && own == OwnKind::Owned,
            CloneAction::Aggregate(id) => {
                let glue = aggregate_glue(module, id)?;
                glue.ty == *ty
                    && glue.own == OwnKind::Owned
                    && own == OwnKind::Owned
                    && glue.fields.iter().all(|field| field.clone.is_some())
            }
            CloneAction::Vector(id) | CloneAction::Array(id) => {
                let glue = vector_glue(module, id)?;
                let fixed = matches!(ty, ResolvedTy::Array(_, _));
                fixed == matches!(action, CloneAction::Array(_))
                    && glue.ty == *ty
                    && own == OwnKind::Owned
                    && glue.element.clone.is_some()
            }
            CloneAction::Map(id) => {
                let glue = map_glue(module, id)?;
                glue.ty == *ty
                    && own == OwnKind::Owned
                    && glue.key.clone.is_some()
                    && glue.value.clone.is_some()
            }
            CloneAction::Set(id) => {
                let glue = set_glue(module, id)?;
                glue.ty == *ty && own == OwnKind::Owned && glue.element.clone.is_some()
            }
            CloneAction::Variant(id) => {
                let glue = variant_glue(module, id)?;
                glue.ty == *ty
                    && glue.own == OwnKind::Owned
                    && own == OwnKind::Owned
                    && glue
                        .variants
                        .iter()
                        .flat_map(|variant| &variant.fields)
                        .all(|field| field.clone.is_some())
            }
        };
    if valid {
        Ok(())
    } else {
        Err(PhysicalError::new(format!(
            "physical clone action {action:?} disagrees with `{}` storage",
            ty.user_facing()
        )))
    }
}

pub(crate) fn verify_destroy_action(
    module: &PhysicalModule,
    ty: &ResolvedTy,
    own: OwnKind,
    action: DestroyAction,
) -> Result<(), PhysicalError> {
    let own_from_facts = OwnKind::of_class(semantic_type_facts(module, ty)?.class);
    let valid = own == own_from_facts
        && match action {
            DestroyAction::Resource(id) => {
                own == OwnKind::Owned
                    && module
                        .resources
                        .get(id.0 as usize)
                        .is_some_and(|resource| resource.ty == *ty)
            }
            DestroyAction::Encoding(format) => {
                own == OwnKind::Owned && encoding_format(ty) == Some(format)
            }
            DestroyAction::Callable => {
                own == OwnKind::Owned
                    && matches!(ty, ResolvedTy::Function { .. } | ResolvedTy::Closure { .. })
            }
            DestroyAction::TraitObject => {
                own == OwnKind::Owned && matches!(ty, ResolvedTy::TraitObject { .. })
            }
            DestroyAction::StringRelease => ty == &ResolvedTy::String && own == OwnKind::Owned,
            DestroyAction::BytesRelease => ty == &ResolvedTy::Bytes && own == OwnKind::Owned,
            DestroyAction::RcRelease(id) => {
                let glue = shared_glue(module, id)?;
                glue.ty == *ty
                    && own == OwnKind::Owned
                    && glue.payload.destroy.is_some() == (glue.payload.own == OwnKind::Owned)
            }
            DestroyAction::WeakRelease => ty.is_builtin(BuiltinType::Weak) && own == OwnKind::Owned,
            DestroyAction::Aggregate(id) => {
                let glue = aggregate_glue(module, id)?;
                glue.ty == *ty
                    && glue.own == OwnKind::Owned
                    && own == OwnKind::Owned
                    && glue
                        .fields
                        .iter()
                        .all(|field| field.own != OwnKind::Owned || field.destroy.is_some())
            }
            DestroyAction::Vector(id) | DestroyAction::Array(id) => {
                let glue = vector_glue(module, id)?;
                matches!(ty, ResolvedTy::Array(_, _)) == matches!(action, DestroyAction::Array(_))
                    && glue.ty == *ty
                    && own == OwnKind::Owned
                    && (glue.element.own != OwnKind::Owned || glue.element.destroy.is_some())
            }
            DestroyAction::Map(id) => {
                let glue = map_glue(module, id)?;
                glue.ty == *ty
                    && own == OwnKind::Owned
                    && [&glue.key, &glue.value]
                        .iter()
                        .all(|recipe| recipe.own != OwnKind::Owned || recipe.destroy.is_some())
            }
            DestroyAction::Set(id) => {
                let glue = set_glue(module, id)?;
                glue.ty == *ty
                    && own == OwnKind::Owned
                    && (glue.element.own != OwnKind::Owned || glue.element.destroy.is_some())
            }
            DestroyAction::Variant(id) => {
                let glue = variant_glue(module, id)?;
                glue.ty == *ty
                    && glue.own == OwnKind::Owned
                    && own == OwnKind::Owned
                    && glue
                        .variants
                        .iter()
                        .flat_map(|variant| &variant.fields)
                        .all(|field| field.own != OwnKind::Owned || field.destroy.is_some())
            }
        };
    if valid {
        Ok(())
    } else {
        Err(PhysicalError::new(format!(
            "physical destroy action {action:?} disagrees with `{}` storage",
            ty.user_facing()
        )))
    }
}

pub(crate) fn verify_aggregate_make(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    dest: StorageId,
    fields: &[StorageId],
    glue: PhysicalAggregateId,
) -> Result<(), PhysicalError> {
    let destination = storage(function, dest)?;
    let recipe = aggregate_glue(module, glue)?;
    if destination.ty != recipe.ty || destination.own != recipe.own {
        return Err(PhysicalError::new(
            "physical aggregate construction destination disagrees with its glue recipe",
        ));
    }
    if fields.len() != recipe.fields.len() {
        return Err(PhysicalError::new(format!(
            "physical aggregate construction has {} fields for {} recipes",
            fields.len(),
            recipe.fields.len()
        )));
    }
    if fields.contains(&dest) {
        return Err(PhysicalError::new(
            "physical aggregate construction aliases its destination storage",
        ));
    }
    let mut consumed = BTreeSet::new();
    for (index, (field, expected)) in fields.iter().zip(&recipe.fields).enumerate() {
        let field = storage(function, *field)?;
        if field.ty != expected.ty || field.own != expected.own {
            return Err(PhysicalError::new(format!(
                "physical aggregate construction field {index} disagrees with its glue recipe"
            )));
        }
        if expected.own == OwnKind::Owned && !consumed.insert(field.id) {
            return Err(PhysicalError::new(format!(
                "physical aggregate construction consumes owned field {index} more than once"
            )));
        }
    }
    Ok(())
}

pub(crate) fn aggregate_projection_field<'a>(
    module: &'a PhysicalModule,
    function: &PhysicalFunction,
    aggregate: StorageId,
    field: u32,
    glue: PhysicalAggregateId,
) -> Result<&'a PhysicalValueRecipe, PhysicalError> {
    let aggregate = storage(function, aggregate)?;
    let recipe = aggregate_glue(module, glue)?;
    let source_own_matches = aggregate.own == recipe.own
        || (recipe.own == OwnKind::Owned && aggregate.own == OwnKind::Guaranteed);
    if aggregate.ty != recipe.ty || !source_own_matches {
        return Err(PhysicalError::new(
            "physical aggregate projection source disagrees with its glue recipe",
        ));
    }
    usize::try_from(field)
        .ok()
        .and_then(|index| recipe.fields.get(index))
        .ok_or_else(|| {
            PhysicalError::new(format!(
                "physical aggregate projection index {field} is out of bounds"
            ))
        })
}

pub(crate) fn verify_aggregate_project_copy(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    dest: StorageId,
    aggregate: StorageId,
    field: u32,
    glue: PhysicalAggregateId,
    action: CloneAction,
) -> Result<(), PhysicalError> {
    let destination = storage(function, dest)?;
    let expected = aggregate_projection_field(module, function, aggregate, field, glue)?;
    if destination.ty != expected.ty
        || destination.own != expected.own
        || expected.clone != Some(action)
    {
        return Err(PhysicalError::new(
            "physical aggregate projection disagrees with its field copy recipe",
        ));
    }
    verify_clone_action(module, &destination.ty, destination.own, action)
}

pub(crate) fn verify_borrow_dependency(
    function: &PhysicalFunction,
    dest: StorageId,
    source: StorageId,
) -> Result<(), PhysicalError> {
    let destination = storage(function, dest)?;
    if destination.own != OwnKind::Guaranteed
        || destination.borrow_parent != Some(source)
        || !matches!(
            storage(function, source)?.own,
            OwnKind::Owned | OwnKind::Guaranteed
        )
    {
        return Err(PhysicalError::new(
            "physical borrow disagrees with its SIR loan dependency",
        ));
    }
    Ok(())
}

pub(crate) fn verify_aggregate_project_borrow(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    dest: StorageId,
    aggregate: StorageId,
    field: u32,
    glue: PhysicalAggregateId,
) -> Result<(), PhysicalError> {
    let destination = storage(function, dest)?;
    let expected = aggregate_projection_field(module, function, aggregate, field, glue)?;
    if destination.ty != expected.ty || expected.own != OwnKind::Owned {
        return Err(PhysicalError::new(
            "physical borrowed projection disagrees with its owning field recipe",
        ));
    }
    verify_borrow_dependency(function, dest, aggregate)
}

pub(crate) fn verify_aggregate_destructure(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    aggregate: StorageId,
    fields: &[StorageId],
    glue: PhysicalAggregateId,
) -> Result<(), PhysicalError> {
    let aggregate = storage(function, aggregate)?;
    let recipe = aggregate_glue(module, glue)?;
    if aggregate.ty != recipe.ty || aggregate.own != recipe.own {
        return Err(PhysicalError::new(
            "physical aggregate destructure source disagrees with its glue recipe",
        ));
    }
    if fields.len() != recipe.fields.len() {
        return Err(PhysicalError::new(format!(
            "physical aggregate destructure has {} results for {} fields",
            fields.len(),
            recipe.fields.len()
        )));
    }
    for (index, (field, expected)) in fields.iter().zip(&recipe.fields).enumerate() {
        let field = storage(function, *field)?;
        if field.ty != expected.ty || field.own != expected.own {
            return Err(PhysicalError::new(format!(
                "physical aggregate destructure field {index} disagrees with its glue recipe"
            )));
        }
    }
    Ok(())
}

/// The exact case recipe of one enum storage a probe reads or a take consumes.
/// A borrowed source may only carry an owning recipe.
pub(crate) fn variant_case<'a>(
    module: &'a PhysicalModule,
    function: &PhysicalFunction,
    source: StorageId,
    variant: u32,
    glue: PhysicalVariantId,
) -> Result<&'a PhysicalVariantCase, PhysicalError> {
    let source = storage(function, source)?;
    let recipe = variant_glue(module, glue)?;
    let source_own_matches = source.own == recipe.own
        || (recipe.own == OwnKind::Owned && source.own == OwnKind::Guaranteed);
    if source.ty != recipe.ty || !source_own_matches {
        return Err(PhysicalError::new(
            "physical variant source disagrees with its glue recipe",
        ));
    }
    usize::try_from(variant)
        .ok()
        .and_then(|index| recipe.variants.get(index))
        .ok_or_else(|| {
            PhysicalError::new(format!("physical variant tag {variant} is out of bounds"))
        })
}

pub(crate) fn variant_projection_field<'a>(
    module: &'a PhysicalModule,
    function: &PhysicalFunction,
    source: StorageId,
    variant: u32,
    field: u32,
    glue: PhysicalVariantId,
) -> Result<&'a PhysicalValueRecipe, PhysicalError> {
    let case = variant_case(module, function, source, variant, glue)?;
    usize::try_from(field)
        .ok()
        .and_then(|index| case.fields.get(index))
        .ok_or_else(|| {
            PhysicalError::new(format!(
                "physical variant projection index {field} is out of bounds"
            ))
        })
}

pub(crate) fn verify_variant_make(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    dest: StorageId,
    variant: u32,
    fields: &[StorageId],
    glue: PhysicalVariantId,
) -> Result<(), PhysicalError> {
    let destination = storage(function, dest)?;
    let recipe = variant_glue(module, glue)?;
    if destination.ty != recipe.ty || destination.own != recipe.own {
        return Err(PhysicalError::new(
            "physical variant construction destination disagrees with its glue recipe",
        ));
    }
    let case = usize::try_from(variant)
        .ok()
        .and_then(|variant| recipe.variants.get(variant))
        .ok_or_else(|| {
            PhysicalError::new(format!(
                "physical variant construction tag {variant} is out of bounds"
            ))
        })?;
    if fields.len() != case.fields.len() {
        return Err(PhysicalError::new(format!(
            "physical variant construction has {} fields for {} recipes",
            fields.len(),
            case.fields.len()
        )));
    }
    if fields.contains(&dest) {
        return Err(PhysicalError::new(
            "physical variant construction aliases its destination storage",
        ));
    }
    let mut consumed = BTreeSet::new();
    for (index, (field, expected)) in fields.iter().zip(&case.fields).enumerate() {
        let field = storage(function, *field)?;
        if field.ty != expected.ty || field.own != expected.own {
            return Err(PhysicalError::new(format!(
                "physical variant construction field {index} disagrees with its glue recipe"
            )));
        }
        if expected.own == OwnKind::Owned && !consumed.insert(field.id) {
            return Err(PhysicalError::new(format!(
                "physical variant construction consumes owned field {index} more than once"
            )));
        }
    }
    Ok(())
}

pub(crate) fn verify_tuple_make(
    function: &PhysicalFunction,
    dest: StorageId,
    elements: &[StorageId],
) -> Result<(), PhysicalError> {
    let destination = storage(function, dest)?;
    let ResolvedTy::Tuple(field_types) = &destination.ty else {
        return Err(PhysicalError::new(
            "physical tuple construction has a non-tuple destination",
        ));
    };
    if destination.own != OwnKind::None {
        return Err(PhysicalError::new(
            "physical tuple construction is limited to no-drop values",
        ));
    }
    if field_types.len() != elements.len() {
        return Err(PhysicalError::new(format!(
            "physical tuple construction has {} elements for {} fields",
            elements.len(),
            field_types.len()
        )));
    }
    for (index, (element, expected)) in elements.iter().zip(field_types).enumerate() {
        let element = storage(function, *element)?;
        if element.own != OwnKind::None || &element.ty != expected {
            return Err(PhysicalError::new(format!(
                "physical tuple element {index} disagrees with its no-drop field type"
            )));
        }
    }
    Ok(())
}

pub(crate) fn verify_tuple_get(
    function: &PhysicalFunction,
    dest: StorageId,
    tuple: StorageId,
    index: u32,
) -> Result<(), PhysicalError> {
    let destination = storage(function, dest)?;
    let tuple = storage(function, tuple)?;
    let ResolvedTy::Tuple(field_types) = &tuple.ty else {
        return Err(PhysicalError::new(
            "physical tuple projection reads a non-tuple value",
        ));
    };
    let field = usize::try_from(index)
        .ok()
        .and_then(|index| field_types.get(index))
        .ok_or_else(|| {
            PhysicalError::new(format!(
                "physical tuple projection index {index} is out of bounds"
            ))
        })?;
    if tuple.own != OwnKind::None || destination.own != OwnKind::None || &destination.ty != field {
        return Err(PhysicalError::new(
            "physical tuple projection disagrees with its no-drop field type",
        ));
    }
    Ok(())
}
