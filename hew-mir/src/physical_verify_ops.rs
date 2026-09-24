//! Independent verification of per-operation storage effects, flow state and value initialization.

use super::{
    callable, defer, generators, map_glue, merge_flow, partial, require_same_storage_type,
    semantic_type_facts, set_glue, shared_glue, storage, terminator_successors, variant_case,
    variant_projection_field, vector_glue, verify_aggregate_destructure, verify_aggregate_make,
    verify_aggregate_project_borrow, verify_aggregate_project_copy, verify_borrow_dependency,
    verify_clone_action, verify_destroy_action, verify_tuple_get, verify_tuple_make,
    verify_value_recipe, verify_variant_make, ArgumentTransfer, BTreeMap, BTreeSet, BinaryOp,
    BlockId, DestroyAction, OwnKind, PhysicalConst, PhysicalEdge, PhysicalError, PhysicalFunction,
    PhysicalLayout, PhysicalModule, PhysicalOp, PhysicalRepr, PhysicalRuntimeAction,
    PhysicalRuntimeCarrier, ResolvedTy, StorageId, StorageOrigin,
};

#[allow(
    clippy::too_many_lines,
    reason = "keep the exhaustive physical operation contract together"
)]
pub(crate) fn verify_operation_storage(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    operation: &PhysicalOp,
) -> Result<(), PhysicalError> {
    match operation {
        PhysicalOp::GeneratorMake { .. } => generators::verify_make(module, function, operation)?,
        PhysicalOp::GeneratorCoerce { dest, source } => {
            generators::verify_coerce(module, function, *dest, *source)?;
        }
        PhysicalOp::StreamPipe {
            capacity,
            stream,
            sink,
            element,
        } => {
            let stream = storage(function, *stream)?;
            let sink = storage(function, *sink)?;
            if *capacity == 0
                || stream.own != OwnKind::Owned
                || sink.own != OwnKind::Owned
                || hew_sir::pipe_parts(&stream.ty, &sink.ty) != Some(&element.ty)
            {
                return Err(PhysicalError::new(
                    "stream pipe halves disagree with their owned element contract",
                ));
            }
            verify_value_recipe(module, element)?;
        }
        PhysicalOp::TaskScopeEnter { duration, .. } => {
            if let Some(duration) = duration {
                if storage(function, *duration)?.ty != ResolvedTy::Duration {
                    return Err(PhysicalError::new(
                        "scope deadline requires Duration storage",
                    ));
                }
            }
        }
        PhysicalOp::TaskScopeClose { .. } => {}
        PhysicalOp::TaskSpawn {
            callable,
            dest,
            output,
            ..
        } => {
            let input = storage(function, *callable)?;
            let result = storage(function, *dest)?;
            let (params, ret, capabilities) =
                hew_sir::callable_parts(&input.ty).map_err(PhysicalError::new)?;
            if !params.is_empty()
                || capabilities.call != hew_types::CallableCallMode::Once
                || result.ty != ResolvedTy::Task(Box::new(ret.clone()))
            {
                return Err(PhysicalError::new(
                    "task spawn disagrees with its callable/result contract",
                ));
            }
            match output {
                Some(output) if &output.ty == ret && *ret != ResolvedTy::Never => {
                    verify_value_recipe(module, output)?;
                }
                None if *ret == ResolvedTy::Never => {}
                _ => {
                    return Err(PhysicalError::new(
                        "task result recipe differs from its callable result",
                    ))
                }
            }
        }
        PhysicalOp::RegisterDefer { dependencies, .. } => {
            for dependency in dependencies {
                storage(function, *dependency)?;
            }
        }
        operation @ (PhysicalOp::FunctionMake { .. }
        | PhysicalOp::ClosureMake { .. }
        | PhysicalOp::DynMake { .. }
        | PhysicalOp::CallableCoerce { .. }) => {
            callable::verify_operation(module, function, operation)?;
        }
        PhysicalOp::Const { dest, value } => {
            verify_constant(module, function, *dest, value)?;
        }
        PhysicalOp::StorageLive { storage: dest } => {
            partial::require_local(function, *dest)?;
        }
        PhysicalOp::Unary { dest, source, .. } => {
            require_same_storage_type(function, *dest, *source, "physical operation")?;
        }
        PhysicalOp::Transfer { dest, source } => {
            verify_transfer(function, *dest, *source)?;
        }
        PhysicalOp::Borrow { dest, source } => {
            verify_whole_value_borrow(module, function, *dest, *source)?;
        }
        PhysicalOp::Cast { dest, source, .. } => {
            storage(function, *dest)?;
            storage(function, *source)?;
        }
        PhysicalOp::TupleMake { dest, elements } => verify_tuple_make(function, *dest, elements)?,
        PhysicalOp::TupleGet { dest, tuple, index } => {
            verify_tuple_get(function, *dest, *tuple, *index)?;
        }
        PhysicalOp::ArrayMake { dest, fields, glue } => {
            let descriptor = vector_glue(module, *glue)?;
            let ResolvedTy::Array(element, length) = &descriptor.ty else {
                return Err(PhysicalError::new("array.make has a non-array descriptor"));
            };
            if storage(function, *dest)?.ty != descriptor.ty
                || storage(function, *dest)?.own != OwnKind::Owned
                || fields.contains(dest)
                || usize::try_from(*length).ok() != Some(fields.len())
            {
                return Err(PhysicalError::new(
                    "array.make length or result differs from its descriptor",
                ));
            }
            let mut consumed = BTreeSet::new();
            for field in fields {
                let field = storage(function, *field)?;
                if field.ty != **element
                    || field.own != descriptor.element.own
                    || (field.own == OwnKind::Owned && !consumed.insert(field.id))
                {
                    return Err(PhysicalError::new(
                        "array.make element differs from its descriptor",
                    ));
                }
            }
        }
        PhysicalOp::ArrayRepeat { dest, seed, glue } => {
            let descriptor = vector_glue(module, *glue)?;
            let ResolvedTy::Array(element, length) = &descriptor.ty else {
                return Err(PhysicalError::new(
                    "array.repeat has a non-array descriptor",
                ));
            };
            if storage(function, *dest)?.ty != descriptor.ty
                || storage(function, *dest)?.own != OwnKind::Owned
                || dest == seed
                || storage(function, *seed)?.ty != **element
                || storage(function, *seed)?.own != descriptor.element.own
                || *length == 0
                || (*length > 1 && descriptor.element.clone.is_none())
            {
                return Err(PhysicalError::new(
                    "array.repeat seed, length or copy recipe differs from its descriptor",
                ));
            }
        }
        PhysicalOp::AggregateMake { dest, fields, glue } => {
            verify_aggregate_make(module, function, *dest, fields, *glue)?;
        }
        PhysicalOp::AggregateProjectCopy {
            dest,
            aggregate,
            field,
            glue,
            action,
        } => verify_aggregate_project_copy(
            module, function, *dest, *aggregate, *field, *glue, *action,
        )?,
        PhysicalOp::AggregateProjectBorrow {
            dest,
            aggregate,
            field,
            glue,
        } => verify_aggregate_project_borrow(module, function, *dest, *aggregate, *field, *glue)?,
        PhysicalOp::AggregateDestructure {
            aggregate,
            fields,
            glue,
        } => verify_aggregate_destructure(module, function, *aggregate, fields, *glue)?,
        PhysicalOp::VariantMake {
            dest,
            variant,
            fields,
            glue,
        } => verify_variant_make(module, function, *dest, *variant, fields, *glue)?,
        PhysicalOp::VariantIs {
            dest,
            source,
            variant,
            glue,
        } => {
            variant_case(module, function, *source, *variant, *glue)?;
            if storage(function, *dest)?.ty != ResolvedTy::Bool {
                return Err(PhysicalError::new(
                    "physical variant test must produce a boolean",
                ));
            }
        }
        PhysicalOp::VariantProjectCopy {
            dest,
            source,
            variant,
            field,
            glue,
            action,
        } => {
            let destination = storage(function, *dest)?;
            let expected =
                variant_projection_field(module, function, *source, *variant, *field, *glue)?;
            if destination.ty != expected.ty
                || destination.own != expected.own
                || expected.clone != Some(*action)
            {
                return Err(PhysicalError::new(
                    "physical variant projection disagrees with its field copy recipe",
                ));
            }
            verify_clone_action(module, &destination.ty, destination.own, *action)?;
        }
        PhysicalOp::VariantProjectBorrow {
            dest,
            source,
            variant,
            field,
            glue,
        } => {
            let destination = storage(function, *dest)?;
            let expected =
                variant_projection_field(module, function, *source, *variant, *field, *glue)?;
            if destination.ty != expected.ty || expected.own != OwnKind::Owned {
                return Err(PhysicalError::new(
                    "physical borrowed variant projection disagrees with its owning field recipe",
                ));
            }
            verify_borrow_dependency(function, *dest, *source)?;
        }
        PhysicalOp::VariantDestructure {
            source,
            variant,
            fields,
            glue,
        } => {
            let case = variant_case(module, function, *source, *variant, *glue)?;
            if fields.len() != case.fields.len() {
                return Err(PhysicalError::new(format!(
                    "physical variant destructure has {} results for {} fields",
                    fields.len(),
                    case.fields.len()
                )));
            }
            for (index, (field, expected)) in fields.iter().zip(&case.fields).enumerate() {
                let field = storage(function, *field)?;
                if field.ty != expected.ty || field.own != expected.own {
                    return Err(PhysicalError::new(format!(
                        "physical variant destructure field {index} disagrees with its glue recipe"
                    )));
                }
            }
        }
        PhysicalOp::Binary { dest, op, lhs, rhs } => {
            verify_binary(function, *dest, *op, *lhs, *rhs)?;
        }
        PhysicalOp::Destroy { source, action, .. } => {
            let source = storage(function, *source)?;
            verify_destroy_action(module, &source.ty, source.own, *action)?;
        }
        PhysicalOp::EndBorrow { source } => {
            let source = storage(function, *source)?;
            if source.own != OwnKind::Guaranteed || source.borrow_parent.is_none() {
                return Err(PhysicalError::new(
                    "physical end-borrow requires a local SIR loan",
                ));
            }
        }
        PhysicalOp::Clone {
            dest,
            source,
            action,
        } => {
            require_same_storage_type(function, *dest, *source, "physical clone")?;
            let destination = storage(function, *dest)?;
            verify_clone_action(module, &destination.ty, destination.own, *action)?;
        }
        PhysicalOp::Assign {
            dest,
            source,
            destroy_old,
            ..
        } => {
            require_same_storage_type(function, *dest, *source, "physical assignment")?;
            let destination = storage(function, *dest)?;
            partial::verify_optional_destroy(module, destination, *destroy_old)?;
        }
        PhysicalOp::StorageDead {
            storage: id,
            destroy,
            ..
        } => {
            partial::require_local(function, *id)?;
            partial::verify_optional_destroy(module, storage(function, *id)?, *destroy)?;
        }
    }
    Ok(())
}

pub(crate) fn verify_transfer(
    function: &PhysicalFunction,
    dest: StorageId,
    source: StorageId,
) -> Result<(), PhysicalError> {
    require_same_storage_type(function, dest, source, "physical transfer")?;
    let source = storage(function, source)?;
    let dest = storage(function, dest)?;
    if source.own != dest.own || source.own == OwnKind::Guaranteed {
        return Err(PhysicalError::new(
            "physical transfer cannot change ownership or move a loan",
        ));
    }
    Ok(())
}

pub(crate) fn verify_whole_value_borrow(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    dest: StorageId,
    source: StorageId,
) -> Result<(), PhysicalError> {
    require_same_storage_type(function, dest, source, "physical borrow")?;
    let ty = &storage(function, source)?.ty;
    if OwnKind::of_class(semantic_type_facts(module, ty)?.class) != OwnKind::Owned {
        return Err(PhysicalError::new(
            "physical borrow requires an owning value",
        ));
    }
    verify_borrow_dependency(function, dest, source)
}

/// Width in bits of an integer destination layout, or `None` when the layout
/// is not an integer.
pub(crate) fn integer_width_bits(layout: &PhysicalLayout) -> Option<u16> {
    match layout.repr {
        PhysicalRepr::Integer { bits } => Some(bits),
        _ => None,
    }
}

/// Inclusive mathematical range a `width`-bit integer destination admits.
pub(crate) fn integer_bit_range(width: u16, signed: bool) -> (i128, i128) {
    if signed {
        let magnitude = 1i128 << (width - 1);
        (-magnitude, magnitude - 1)
    } else {
        (0, (1i128 << width) - 1)
    }
}

/// Keep only the low `width` bits of a bit pattern.
pub(crate) fn mask_to_width(bits: u64, width: u16) -> u64 {
    if width >= 64 {
        bits
    } else {
        bits & ((1u64 << width) - 1)
    }
}

/// True when no bit above the destination width is set. A constant that
/// carries stray high bits is malformed IR: the backend emits the pattern as
/// written and would silently produce a different value.
pub(crate) fn canonical_integer_bits(bits: u64, width: u16) -> bool {
    mask_to_width(bits, width) == bits
}

pub(crate) fn verify_constant(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    dest: StorageId,
    value: &PhysicalConst,
) -> Result<(), PhysicalError> {
    let destination = storage(function, dest)?;
    let matches_destination = match value {
        PhysicalConst::ActorIngressAdapter(adapter) => {
            adapter
                .handler(&module.actors)
                .map_err(PhysicalError::new)?;
            destination.ty == hew_sir::ActorIngressAdapter::pointer_type()
                && destination.own == OwnKind::None
                && destination.layout.repr == PhysicalRepr::Pointer
        }
        PhysicalConst::IntegerBits(bits) => {
            destination.ty.is_integer()
                && destination.own == OwnKind::None
                && integer_width_bits(&destination.layout)
                    .is_some_and(|width| canonical_integer_bits(*bits, width))
        }
        PhysicalConst::Bool(_) => {
            destination.ty == ResolvedTy::Bool && destination.own == OwnKind::None
        }
        PhysicalConst::Float(_) => destination.ty.is_float() && destination.own == OwnKind::None,
        PhysicalConst::Char(_) => {
            destination.ty == ResolvedTy::Char && destination.own == OwnKind::None
        }
        PhysicalConst::Unit => {
            destination.ty == ResolvedTy::Unit && destination.own == OwnKind::None
        }
        PhysicalConst::Duration(_) => {
            destination.ty == ResolvedTy::Duration && destination.own == OwnKind::None
        }
        PhysicalConst::String(id) => {
            if !module.string_literals.contains_key(id) {
                return Err(PhysicalError::new(format!(
                    "physical string constant references unknown literal {}",
                    id.0
                )));
            }
            destination.ty == ResolvedTy::String && destination.own == OwnKind::Owned
        }
        PhysicalConst::Bytes(id) => {
            if !module.bytes_literals.contains_key(id) {
                return Err(PhysicalError::new(format!(
                    "physical bytes constant references unknown literal {}",
                    id.0
                )));
            }
            destination.ty == ResolvedTy::Bytes && destination.own == OwnKind::Owned
        }
    };
    if !matches_destination {
        return Err(PhysicalError::new(
            "physical constant payload disagrees with destination type or ownership",
        ));
    }
    Ok(())
}

pub(crate) fn verify_binary(
    function: &PhysicalFunction,
    dest: StorageId,
    op: BinaryOp,
    lhs: StorageId,
    rhs: StorageId,
) -> Result<(), PhysicalError> {
    let destination = storage(function, dest)?;
    let left = storage(function, lhs)?;
    let right = storage(function, rhs)?;
    if left.ty != right.ty
        || left.own != OwnKind::None
        || right.own != OwnKind::None
        || destination.own != OwnKind::None
    {
        return Err(PhysicalError::new(
            "physical binary operation uses incompatible storage types or ownership",
        ));
    }
    let valid_result = match op {
        BinaryOp::Equal
        | BinaryOp::NotEqual
        | BinaryOp::Less
        | BinaryOp::LessEqual
        | BinaryOp::Greater
        | BinaryOp::GreaterEqual => destination.ty == ResolvedTy::Bool,
        BinaryOp::And | BinaryOp::Or | BinaryOp::Range | BinaryOp::RangeInclusive => false,
        BinaryOp::Add
        | BinaryOp::Subtract
        | BinaryOp::Multiply
        | BinaryOp::Divide
        | BinaryOp::Modulo
        | BinaryOp::BitAnd
        | BinaryOp::BitOr
        | BinaryOp::BitXor
        | BinaryOp::Shl
        | BinaryOp::Shr
        | BinaryOp::WrappingAdd
        | BinaryOp::WrappingSub
        | BinaryOp::WrappingMul => destination.ty == left.ty,
    };
    if !valid_result {
        return Err(PhysicalError::new(
            "physical binary result type disagrees with its operation",
        ));
    }
    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum InitState {
    Uninitialized,
    Initialized,
    MaybeInitialized,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FlowState {
    pub(crate) slots: Vec<InitState>,
    pub(crate) active: Vec<InitState>,
    pub(crate) fault: FaultState,
    pub(crate) exit: u8,
    pub(crate) defers: defer::State,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum FaultState {
    None,
    Active,
    MaybeActive,
}

pub(crate) fn verify_initialization(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    cleanup_needs_fault: Option<&BTreeSet<BlockId>>,
) -> Result<(), PhysicalError> {
    let defer_plan = defer::verify_regions(function)?;
    defer::verify_calls(module, function, &defer_plan)?;
    let blocks = function
        .blocks
        .iter()
        .map(|block| (block.id, block))
        .collect::<BTreeMap<_, _>>();
    let mut entry = FlowState {
        slots: vec![InitState::Uninitialized; function.storage.len()],
        active: vec![InitState::Uninitialized; function.storage.len()],
        fault: FaultState::None,
        exit: defer::ORDINARY,
        defers: defer::State::default(),
    };
    for parameter in &function.parameters {
        *entry.slots.get_mut(parameter.0 as usize).ok_or_else(|| {
            PhysicalError::new(format!("unknown physical storage {}", parameter.0))
        })? = InitState::Initialized;
        partial::set_leaves(function, &mut entry, *parameter, InitState::Initialized);
    }

    for slot in &function.storage {
        if matches!(
            slot.origin,
            StorageOrigin::Capture { .. }
                | StorageOrigin::ActorState {
                    initialized: true,
                    ..
                }
        ) {
            entry.slots[slot.id.0 as usize] = InitState::Initialized;
        }
    }
    let borrows = &BorrowDependents::of(function);
    let mut incoming = BTreeMap::from([(function.entry, vec![entry])]);
    let mut pending = vec![function.entry];
    while let Some(block_id) = pending.pop() {
        let block = blocks.get(&block_id).ok_or_else(|| {
            PhysicalError::new(format!("physical CFG has no block {}", block_id.0))
        })?;
        for mut state in incoming[&block_id].clone() {
            defer::verify_entry_phase(&defer_plan, block_id, &state)?;
            for operation in &block.ops {
                if cleanup_needs_fault.is_some_and(|blocks| blocks.contains(&block_id))
                    && (state.exit == 0 || state.exit & defer::ORDINARY != 0)
                    && matches!(operation, PhysicalOp::Destroy { cleanup, .. } | PhysicalOp::StorageDead { cleanup, .. }
                        if cleanup.mode() == hew_sir::CleanupMode::Trap)
                {
                    return Err(PhysicalError::new(
                        "physical trap-only cleanup lost its fault exit cause",
                    ));
                }
                apply_operation(module, function, borrows, operation, &mut state, block_id)?;
            }
            for (target, successor) in terminator_successors(
                module,
                function,
                borrows,
                &block.terminator,
                state,
                block_id,
                &defer_plan,
            )? {
                let alternatives = incoming.entry(target).or_default();
                if alternatives
                    .iter()
                    .any(|old| !old.defers.same_phase(&successor.defers))
                {
                    return Err(PhysicalError::new(
                        "physical CFG joins incompatible defer phases",
                    ));
                }
                let changed = if let Some(existing) = alternatives.iter_mut().find(|old| {
                    old.fault == successor.fault
                        && old.exit == successor.exit
                        && old.defers.same_faults(&successor.defers)
                }) {
                    merge_flow(existing, &successor)
                } else {
                    alternatives.push(successor);
                    true
                };
                if changed {
                    pending.push(target);
                }
            }
        }
    }
    Ok(())
}

pub(crate) fn initialized(
    function: &PhysicalFunction,
    state: &FlowState,
    id: StorageId,
    block: BlockId,
    context: &str,
) -> Result<(), PhysicalError> {
    if let Some(entry) = function.place_storage.get(&id) {
        partial::require_root(function, state, id, block, context)?;
        for leaf in &entry.leaves {
            initialized_slot(state, leaf.storage, block, context)?;
        }
        Ok(())
    } else {
        initialized_slot(state, id, block, context)
    }
}

pub(crate) fn initialized_slot(
    state: &FlowState,
    id: StorageId,
    block: BlockId,
    context: &str,
) -> Result<(), PhysicalError> {
    let slot = state
        .slots
        .get(id.0 as usize)
        .ok_or_else(|| PhysicalError::new(format!("unknown physical storage {}", id.0)))?;
    match slot {
        InitState::Initialized => Ok(()),
        InitState::Uninitialized => Err(PhysicalError::new(format!(
            "physical bb{} {context} reads uninitialized storage {}",
            block.0, id.0
        ))),
        InitState::MaybeInitialized => Err(PhysicalError::new(format!(
            "physical bb{} {context} reads storage {} that is not initialized on every path",
            block.0, id.0
        ))),
    }
}

pub(crate) fn define(
    function: &PhysicalFunction,
    borrows: &BorrowDependents,
    state: &mut FlowState,
    id: StorageId,
    block: BlockId,
    context: &str,
) -> Result<(), PhysicalError> {
    require_no_live_borrows(function, borrows, state, id)?;
    let own = storage(function, id)?.own;
    if let Some(entry) = function.place_storage.get(&id).filter(|entry| {
        entry.root != id
            || matches!(
                function.storage[id.0 as usize].origin,
                StorageOrigin::Local(_)
            )
    }) {
        partial::require_root(function, state, id, block, context)?;
        if entry
            .leaves
            .iter()
            .any(|leaf| state.slots[leaf.storage.0 as usize] != InitState::Uninitialized)
        {
            return Err(PhysicalError::new(format!(
                "physical bb{} {context} overwrites initialized aggregate contents {}",
                block.0, id.0
            )));
        }
        partial::set_leaves(function, state, id, InitState::Initialized);
        return Ok(());
    }
    let slot = state
        .slots
        .get_mut(id.0 as usize)
        .ok_or_else(|| PhysicalError::new(format!("unknown physical storage {}", id.0)))?;
    match (*slot, own) {
        (InitState::Uninitialized, _) | (_, OwnKind::None) => {}
        (InitState::Initialized, OwnKind::Owned | OwnKind::Guaranteed) => {
            return Err(PhysicalError::new(format!(
                "physical bb{} {context} overwrites initialized storage {}",
                block.0, id.0
            )));
        }
        (InitState::MaybeInitialized, OwnKind::Owned | OwnKind::Guaranteed) => {
            return Err(PhysicalError::new(format!(
                "physical bb{} {context} may overwrite a live obligation in storage {}",
                block.0, id.0
            )));
        }
    }
    *slot = InitState::Initialized;
    partial::set_leaves(function, state, id, InitState::Initialized);
    Ok(())
}

/// Storage that borrows, or projects from, each storage root.
///
/// A slot's borrow parent, capture environment or partition root is the one
/// step upward to what it depends on; this is that relation read downward, so
/// asking whether a live loan depends on one root visits that root's
/// dependants instead of every slot in the function.
pub(crate) struct BorrowDependents {
    children: BTreeMap<StorageId, Vec<StorageId>>,
    /// Capture slots grouped by the environment they belong to, and every
    /// capture slot in declaration order. A closure boundary asks both
    /// questions per operation, which is a scan of the whole function each
    /// time without them.
    captures: BTreeMap<StorageId, Vec<StorageId>>,
    capture_slots: Vec<StorageId>,
}

impl BorrowDependents {
    pub(crate) fn of(function: &PhysicalFunction) -> Self {
        let mut children = BTreeMap::<StorageId, Vec<StorageId>>::new();
        let mut captures = BTreeMap::<StorageId, Vec<StorageId>>::new();
        let mut capture_slots = Vec::new();
        for slot in &function.storage {
            if let StorageOrigin::Capture { environment, .. } = slot.origin {
                captures.entry(environment).or_default().push(slot.id);
                capture_slots.push(slot.id);
            }
            let parent = if let Some(parent) = slot.borrow_parent {
                Some(parent)
            } else if let StorageOrigin::Capture { environment, .. } = slot.origin {
                Some(environment)
            } else {
                function
                    .place_storage
                    .get(&slot.id)
                    .map(|projection| projection.root)
                    .filter(|root| *root != slot.id)
            };
            if let Some(parent) = parent {
                children.entry(parent).or_default().push(slot.id);
            }
        }
        Self {
            children,
            captures,
            capture_slots,
        }
    }

    /// The capture slots of one environment, in declaration order.
    pub(super) fn captures_of(&self, environment: StorageId) -> &[StorageId] {
        self.captures.get(&environment).map_or(&[], Vec::as_slice)
    }

    /// Every capture slot in the function, in declaration order.
    pub(super) fn capture_slots(&self) -> &[StorageId] {
        &self.capture_slots
    }

    /// Whether any loan that depends on `root` still holds a value.
    pub(crate) fn any_live_loan(
        &self,
        function: &PhysicalFunction,
        state: &FlowState,
        source: StorageId,
    ) -> bool {
        let projection = function.place_storage.get(&source);
        let root = projection.map_or(source, |entry| entry.root);
        let mut pending = vec![root];
        let mut seen = BTreeSet::new();
        while let Some(id) = pending.pop() {
            if !seen.insert(id) {
                // Every slot has one parent, so reaching one twice means the
                // table is cyclic. A cycle cannot prove independence from a
                // root, so it counts as a live loan.
                return true;
            }
            for &child in self.children.get(&id).into_iter().flatten() {
                // Canonical projection paths preserve the SIR partition:
                // sibling storage cannot hold a loan of the selected field.
                if let (Some(source), Some(child)) =
                    (projection, function.place_storage.get(&child))
                {
                    if source.root == child.root
                        && !source.path.starts_with(&child.path)
                        && !child.path.starts_with(&source.path)
                    {
                        continue;
                    }
                }
                if function.storage[child.0 as usize].borrow_parent.is_some()
                    && state.slots[child.0 as usize] != InitState::Uninitialized
                {
                    return true;
                }
                pending.push(child);
            }
        }
        false
    }
}

pub(crate) fn require_no_live_borrows(
    function: &PhysicalFunction,
    borrows: &BorrowDependents,
    state: &FlowState,
    source: StorageId,
) -> Result<(), PhysicalError> {
    if borrows.any_live_loan(function, state, source) {
        return Err(PhysicalError::new(format!(
            "physical storage {} cannot end or change while a dependent loan is live",
            source.0
        )));
    }
    Ok(())
}

pub(crate) fn consume_if_owned(
    function: &PhysicalFunction,
    borrows: &BorrowDependents,
    state: &mut FlowState,
    id: StorageId,
) -> Result<(), PhysicalError> {
    // A take may leave a pending action's place empty until it is stored
    // again; the action requires it initialized when it runs.
    if !matches!(
        storage(function, id)?.origin,
        StorageOrigin::Local(_)
            | StorageOrigin::Aggregate(_)
            | StorageOrigin::ActorState { .. }
            | StorageOrigin::Capture { .. }
    ) {
        defer::require_unreserved(function, state, id)?;
    }
    if let Some(entry) = function.place_storage.get(&id) {
        require_no_live_borrows(function, borrows, state, id)?;
        if entry.root == id {
            state.slots[id.0 as usize] = InitState::Uninitialized;
        }
        partial::set_leaves(function, state, id, InitState::Uninitialized);
        return Ok(());
    }
    if storage(function, id)?.own == OwnKind::Owned
        || matches!(
            storage(function, id)?.origin,
            StorageOrigin::Capture { .. } | StorageOrigin::ActorState { .. }
        )
    {
        require_no_live_borrows(function, borrows, state, id)?;
        *state
            .slots
            .get_mut(id.0 as usize)
            .ok_or_else(|| PhysicalError::new(format!("unknown physical storage {}", id.0)))? =
            InitState::Uninitialized;
        callable::invalidate_captures(borrows, state, id);
    }
    Ok(())
}

#[allow(
    clippy::too_many_lines,
    reason = "keep physical content and storage transitions in one exhaustive match"
)]
pub(crate) fn apply_operation(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    borrows: &BorrowDependents,
    operation: &PhysicalOp,
    state: &mut FlowState,
    block: BlockId,
) -> Result<(), PhysicalError> {
    match operation {
        PhysicalOp::RegisterDefer {
            defer,
            scope,
            dependencies,
        } => {
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(
                    "physical registration would abandon an active fault",
                ));
            }
            for dependency in dependencies {
                initialized(function, state, *dependency, block, "defer dependency")?;
            }
            state.defers.register(*defer, *scope, dependencies)?;
        }
        PhysicalOp::TaskScopeEnter { duration, .. } => {
            if let Some(duration) = duration {
                initialized(function, state, *duration, block, "scope deadline")?;
            }
        }
        PhysicalOp::TaskScopeClose { .. } => {}
        PhysicalOp::GeneratorMake { callable, dest, .. }
        | PhysicalOp::TaskSpawn { callable, dest, .. } => {
            initialized(function, state, *callable, block, "task callable")?;
            consume_if_owned(function, borrows, state, *callable)?;
            define(function, borrows, state, *dest, block, "task handle")?;
        }
        PhysicalOp::StreamPipe { stream, sink, .. } => {
            define(function, borrows, state, *stream, block, "stream half")?;
            define(function, borrows, state, *sink, block, "sink half")?;
        }
        PhysicalOp::FunctionMake { dest, .. } | PhysicalOp::Const { dest, .. } => {
            define(function, borrows, state, *dest, block, "constant")?;
        }
        PhysicalOp::StorageLive { storage: dest } => {
            partial::activate(function, state, *dest, block)?;
        }
        PhysicalOp::Unary { dest, source, .. } | PhysicalOp::Cast { dest, source, .. } => {
            initialized(function, state, *source, block, "operation")?;
            define(function, borrows, state, *dest, block, "operation")?;
        }
        PhysicalOp::TupleMake { dest, elements } => {
            for element in elements {
                initialized(function, state, *element, block, "tuple construction")?;
            }
            define(function, borrows, state, *dest, block, "tuple construction")?;
        }
        PhysicalOp::TupleGet { dest, tuple, .. } => {
            initialized(function, state, *tuple, block, "tuple projection")?;
            define(function, borrows, state, *dest, block, "tuple projection")?;
        }
        PhysicalOp::AggregateMake { dest, fields, .. }
        | PhysicalOp::ArrayMake { dest, fields, .. }
        | PhysicalOp::ClosureMake { dest, fields, .. } => {
            for field in fields {
                initialized(function, state, *field, block, "aggregate construction")?;
            }
            define(
                function,
                borrows,
                state,
                *dest,
                block,
                "aggregate construction",
            )?;
            for field in fields {
                consume_if_owned(function, borrows, state, *field)?;
            }
        }
        PhysicalOp::ArrayRepeat { dest, seed, .. } => {
            initialized(function, state, *seed, block, "array repeat seed")?;
            define(function, borrows, state, *dest, block, "array repeat")?;
            consume_if_owned(function, borrows, state, *seed)?;
        }
        PhysicalOp::VariantMake { dest, fields, .. } => {
            for field in fields {
                initialized(function, state, *field, block, "variant construction")?;
            }
            define(
                function,
                borrows,
                state,
                *dest,
                block,
                "variant construction",
            )?;
            for field in fields {
                consume_if_owned(function, borrows, state, *field)?;
            }
        }
        PhysicalOp::AggregateProjectCopy {
            dest, aggregate, ..
        }
        | PhysicalOp::AggregateProjectBorrow {
            dest, aggregate, ..
        }
        | PhysicalOp::VariantIs {
            dest,
            source: aggregate,
            ..
        }
        | PhysicalOp::VariantProjectCopy {
            dest,
            source: aggregate,
            ..
        }
        | PhysicalOp::VariantProjectBorrow {
            dest,
            source: aggregate,
            ..
        } => {
            initialized(function, state, *aggregate, block, "aggregate projection")?;
            define(
                function,
                borrows,
                state,
                *dest,
                block,
                "aggregate projection",
            )?;
        }
        PhysicalOp::VariantDestructure { source, fields, .. } => {
            initialized(function, state, *source, block, "variant destructure")?;
            for field in fields {
                define(
                    function,
                    borrows,
                    state,
                    *field,
                    block,
                    "variant destructure",
                )?;
            }
            consume_if_owned(function, borrows, state, *source)?;
        }
        PhysicalOp::AggregateDestructure {
            aggregate, fields, ..
        } => {
            initialized(function, state, *aggregate, block, "aggregate destructure")?;
            for field in fields {
                define(
                    function,
                    borrows,
                    state,
                    *field,
                    block,
                    "aggregate destructure",
                )?;
            }
            consume_if_owned(function, borrows, state, *aggregate)?;
        }
        PhysicalOp::Binary { dest, lhs, rhs, .. } => {
            initialized(function, state, *lhs, block, "binary operation")?;
            initialized(function, state, *rhs, block, "binary operation")?;
            define(function, borrows, state, *dest, block, "binary operation")?;
        }
        PhysicalOp::Transfer { dest, source }
        | PhysicalOp::CallableCoerce { dest, source }
        | PhysicalOp::GeneratorCoerce { dest, source }
        | PhysicalOp::DynMake { dest, source, .. } => {
            initialized(function, state, *source, block, "transfer")?;
            if dest != source {
                define(function, borrows, state, *dest, block, "transfer")?;
                consume_if_owned(function, borrows, state, *source)?;
            }
        }
        PhysicalOp::Clone { dest, source, .. } | PhysicalOp::Borrow { dest, source } => {
            initialized(function, state, *source, block, "copy or borrow")?;
            define(function, borrows, state, *dest, block, "copy or borrow")?;
        }
        PhysicalOp::Destroy {
            source,
            action,
            cleanup,
        } => {
            defer::require_unreserved(function, state, *source)?;
            partial::require_root(function, state, *source, block, "destroy")?;
            partial::require_droppable(module, function, state, *source, cleanup.mode())?;
            require_no_live_borrows(function, borrows, state, *source)?;
            invalidate_storage(function, borrows, state, *source);
            arm_release_fault(module, function, state, Some(*source), Some(*action));
        }
        PhysicalOp::EndBorrow { source } => {
            initialized(function, state, *source, block, "end-borrow")?;
            require_no_live_borrows(function, borrows, state, *source)?;
            invalidate_storage(function, borrows, state, *source);
        }
        PhysicalOp::Assign {
            dest,
            source,
            destroy_old,
            ..
        } => {
            partial::require_root(function, state, *dest, block, "assignment destination")?;
            initialized(function, state, *source, block, "assignment source")?;
            partial::require_droppable(
                module,
                function,
                state,
                *dest,
                hew_sir::CleanupMode::Ordinary,
            )?;
            require_no_live_borrows(function, borrows, state, *dest)?;
            // `destroy_old` is the whole release here: an assignment replaces
            // the root, and codegen releases exactly what it names.
            arm_release_fault(module, function, state, None, *destroy_old);
            consume_if_owned(function, borrows, state, *source)?;
            partial::set_leaves(function, state, *dest, InitState::Initialized);
        }
        PhysicalOp::StorageDead {
            storage: id,
            destroy,
            cleanup,
        } => {
            defer::require_unreserved(function, state, *id)?;
            partial::require_root(function, state, *id, block, "end-lifetime")?;
            partial::require_droppable(module, function, state, *id, cleanup.mode())?;
            require_no_live_borrows(function, borrows, state, *id)?;
            arm_release_fault(module, function, state, Some(*id), *destroy);
            partial::set_leaves(function, state, *id, InitState::Uninitialized);
            if matches!(
                storage(function, *id)?.origin,
                StorageOrigin::ActorState {
                    initialized: false,
                    ..
                }
            ) {
                // A deferred actor seat (D447) has no partition of its own:
                // ending its lifetime releases the seat itself.
                state.slots[id.0 as usize] = InitState::Uninitialized;
            }
            state.active[id.0 as usize] = InitState::Uninitialized;
        }
    }
    Ok(())
}

/// The receiver recipe whose contents a runtime mutation can release.
pub(crate) fn runtime_receiver_release(
    module: &PhysicalModule,
    action: &PhysicalRuntimeAction,
) -> Result<Option<DestroyAction>, PhysicalError> {
    if !action.family.releases_receiver_contents() {
        return Ok(None);
    }
    Ok(match action.carrier {
        PhysicalRuntimeCarrier::SharedHandle(glue) => {
            shared_glue(module, glue)?;
            Some(DestroyAction::RcRelease(glue))
        }
        PhysicalRuntimeCarrier::Vector { glue, .. } => {
            vector_glue(module, glue)?;
            Some(DestroyAction::Vector(glue))
        }
        PhysicalRuntimeCarrier::Map { glue, .. } => {
            map_glue(module, glue)?;
            Some(DestroyAction::Map(glue))
        }
        PhysicalRuntimeCarrier::Set { glue, .. } => {
            set_glue(module, glue)?;
            Some(DestroyAction::Set(glue))
        }
        _ => {
            return Err(PhysicalError::new(
                "runtime release lacks its owning carrier",
            ))
        }
    })
}

/// Arm the frame's fault slot for a release that can run an authored `close`.
///
/// The release is the frame's fault edge (D516): a failing close fills the
/// frame's fault record, the frame keeps releasing what it still owns, and the
/// outcome leaves through a cleanup dispatch. Marking the fault possible here
/// is what makes every other rule in this verifier - no call, no suspension
/// and no normal return while a fault is owned - hold SIR to emitting that
/// dispatch.
pub(crate) fn arm_release_fault(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    state: &mut FlowState,
    partitioned: Option<StorageId>,
    destroy: Option<DestroyAction>,
) {
    if state.fault != FaultState::None {
        // A release under a fault already owned only adds a secondary line.
        return;
    }
    let raises = |action: &DestroyAction| module.releases.raises_fault(*action);
    if destroy.as_ref().is_some_and(raises)
        || partitioned
            .and_then(|id| function.place_storage.get(&id))
            .is_some_and(|place| {
                place
                    .leaves
                    .iter()
                    .any(|leaf| leaf.destroy.as_ref().is_some_and(raises))
            })
    {
        state.fault = FaultState::MaybeActive;
    }
}

pub(crate) fn invalidate_storage(
    function: &PhysicalFunction,
    borrows: &BorrowDependents,
    state: &mut FlowState,
    id: StorageId,
) {
    state.slots[id.0 as usize] = InitState::Uninitialized;
    partial::set_leaves(function, state, id, InitState::Uninitialized);
    callable::invalidate_captures(borrows, state, id);
}

pub(crate) fn apply_edge(
    function: &PhysicalFunction,
    borrows: &BorrowDependents,
    edge: &PhysicalEdge,
    mut state: FlowState,
    block: BlockId,
) -> Result<(BlockId, FlowState), PhysicalError> {
    let before = state.slots.clone();
    for (source, _) in &edge.transfers {
        partial::require_root(function, &state, *source, block, "edge transfer")?;
        require_no_live_borrows(function, borrows, &state, *source)?;
    }
    // The predecessor is one simultaneous move: a destination may itself
    // supply another destination in a loop permutation.
    for (source, _) in &edge.transfers {
        consume_if_owned(function, borrows, &mut state, *source)?;
        // A loan rename moves the alias: the source name is gone in the
        // successor, so it must not stay live to the function exit.
        if storage(function, *source)?.own == OwnKind::Guaranteed
            && storage(function, *source)?.borrow_parent.is_some()
        {
            invalidate_storage(function, borrows, &mut state, *source);
        }
    }
    for (source, destination) in &edge.transfers {
        if source == destination && storage(function, *source)?.own == OwnKind::None {
            continue;
        }
        define(
            function,
            borrows,
            &mut state,
            *destination,
            block,
            "edge transfer",
        )?;
    }
    for (source, destination) in &edge.leaf_transfers {
        state.slots[destination.0 as usize] = before[source.0 as usize];
    }
    Ok((edge.target, state))
}

#[allow(
    clippy::too_many_arguments,
    reason = "one call boundary threading its argument, result and both edges"
)]
pub(crate) fn call_successors(
    function: &PhysicalFunction,
    borrows: &BorrowDependents,
    args: &[ArgumentTransfer],
    result: Option<StorageId>,
    normal: Option<&PhysicalEdge>,
    unwind: Option<&PhysicalEdge>,
    state: FlowState,
    block: BlockId,
) -> Result<Vec<(BlockId, FlowState)>, PhysicalError> {
    call_successors_with_handback(
        function, borrows, args, result, None, normal, unwind, state, block,
    )
}

/// A `var self` call's unwind edge also defines the receiver its failing
/// callee handed back.
#[allow(
    clippy::too_many_arguments,
    reason = "every invoke-style terminator shares this transfer"
)]
pub(crate) fn call_successors_with_handback(
    function: &PhysicalFunction,
    borrows: &BorrowDependents,
    args: &[ArgumentTransfer],
    result: Option<StorageId>,
    handback: Option<StorageId>,
    normal: Option<&PhysicalEdge>,
    unwind: Option<&PhysicalEdge>,
    mut state: FlowState,
    block: BlockId,
) -> Result<Vec<(BlockId, FlowState)>, PhysicalError> {
    if state.fault != FaultState::None {
        return Err(PhysicalError::new(format!(
            "physical bb{} issues a call while an earlier fault is active",
            block.0
        )));
    }
    for argument in args {
        let (source, moves) = match argument {
            ArgumentTransfer::Borrow(source)
            | ArgumentTransfer::BorrowMut(source)
            | ArgumentTransfer::Clone { source, .. } => (*source, false),
            ArgumentTransfer::Move(source) => (*source, true),
        };
        if matches!(argument, ArgumentTransfer::BorrowMut(_)) {
            require_no_live_borrows(function, borrows, &state, source)?;
        }
        initialized(function, &state, source, block, "call argument")?;
        if storage(function, source)?.own == OwnKind::Guaranteed
            && !matches!(
                argument,
                ArgumentTransfer::Borrow(_) | ArgumentTransfer::BorrowMut(_)
            )
        {
            return Err(PhysicalError::new(
                "physical guaranteed call argument must use its borrow contract",
            ));
        }
        if moves {
            consume_if_owned(function, borrows, &mut state, source)?;
        }
    }
    let mut normal_state = state.clone();
    if let Some(result) = result {
        define(
            function,
            borrows,
            &mut normal_state,
            result,
            block,
            "call result",
        )?;
    }
    let mut successors = Vec::new();
    if let Some(normal) = normal {
        successors.push(apply_edge(function, borrows, normal, normal_state, block)?);
    }
    if let Some(unwind) = unwind {
        let mut failure_state = state;
        if let Some(result) = result {
            failure_state.slots[result.0 as usize] = InitState::Uninitialized;
        }
        failure_state.fault = FaultState::Active;
        failure_state.exit = defer::TRAP;
        if let Some(handback) = handback {
            define(
                function,
                borrows,
                &mut failure_state,
                handback,
                block,
                "receiver handback",
            )?;
        }
        successors.push(apply_edge(function, borrows, unwind, failure_state, block)?);
    }
    Ok(successors)
}
