//! Resumable callable ABI selection from explicit typed control flow.

use super::{CallableId, PhysicalError, PhysicalModule, PhysicalTerminator};
use std::collections::{BTreeMap, BTreeSet};

pub(super) fn verify_task_scopes(function: &super::PhysicalFunction) -> Result<(), PhysicalError> {
    use super::{PhysicalOp, TaskScopeId};
    let mut declared = BTreeSet::new();
    for op in function.blocks.iter().flat_map(|block| &block.ops) {
        if let PhysicalOp::TaskScopeEnter { scope, .. } = op {
            if !declared.insert(*scope) {
                return Err(PhysicalError::new(
                    "physical task scope has duplicate storage declarations",
                ));
            }
        }
    }
    let blocks: BTreeMap<_, _> = function
        .blocks
        .iter()
        .map(|block| (block.id, block))
        .collect();
    let mut states = BTreeMap::new();
    states.insert(function.entry, Vec::<(TaskScopeId, bool)>::new());
    let mut pending = vec![function.entry];
    while let Some(id) = pending.pop() {
        let Some(block) = blocks.get(&id) else {
            continue;
        };
        let mut stack = states[&id].clone();
        for op in &block.ops {
            match op {
                PhysicalOp::TaskScopeEnter { scope, parent, .. } => {
                    if stack.last().map(|(scope, _)| scope) != parent.as_ref()
                        || stack.iter().any(|(active, _)| active == scope)
                    {
                        return Err(PhysicalError::new(
                            "physical task scope has inconsistent cancellation ancestry",
                        ));
                    }
                    stack.push((*scope, false));
                }
                PhysicalOp::TaskSpawn { scope, .. } if stack.last() != Some(&(*scope, false)) => {
                    return Err(PhysicalError::new(
                        "physical task spawn has no active lexical scope",
                    ));
                }
                PhysicalOp::TaskScopeClose { scope } => {
                    let drained = stack.pop();
                    if drained != Some((*scope, true)) {
                        return Err(PhysicalError::new(
                            "physical task scope closes before drain",
                        ));
                    }
                }
                _ => {}
            }
        }
        if let PhysicalTerminator::TaskScopeJoin { scope, .. } = block.terminator {
            if stack.last() != Some(&(scope, false)) {
                return Err(PhysicalError::new(
                    "physical task join lacks its active lexical scope",
                ));
            }
            stack.last_mut().unwrap().1 = true;
        }
        let edges = super::defer::edges(&block.terminator);
        // Ownership lowering keeps impossible fault-dispatch successors as
        // Unreachable blocks. They are not executable scope exits; the fault
        // verifier separately rejects abandoning an active fault there.
        if edges.is_empty()
            && !stack.is_empty()
            && !matches!(block.terminator, PhysicalTerminator::Unreachable)
        {
            return Err(PhysicalError::new(format!(
                "physical exit {id:?} ({:?}) leaves task storage live: {stack:?}",
                block.terminator
            )));
        }
        for edge in edges {
            if let Some(previous) = states.get(&edge.target) {
                if previous != &stack {
                    return Err(PhysicalError::new(
                        "physical merge disagrees on task scope lifetime",
                    ));
                }
            } else {
                states.insert(edge.target, stack.clone());
                pending.push(edge.target);
            }
        }
    }
    Ok(())
}

fn close_callers(
    mut resumable: BTreeSet<CallableId>,
    calls: &BTreeMap<CallableId, Vec<CallableId>>,
) -> BTreeSet<CallableId> {
    loop {
        let mut changed = false;
        for (caller, callees) in calls {
            if callees.iter().any(|callee| resumable.contains(callee)) {
                changed |= resumable.insert(*caller);
            }
        }
        if !changed {
            return resumable;
        }
    }
}

fn semantic_value_callees(
    module: &hew_sir::SemModule,
    ty: &hew_types::ResolvedTy,
    capability: hew_types::ValueCapability,
) -> BTreeSet<CallableId> {
    let mut pending = vec![ty.clone()];
    let mut seen = BTreeSet::new();
    let mut callees = BTreeSet::new();
    while let Some(ty) = pending.pop() {
        if !seen.insert(ty.clone()) {
            continue;
        }
        let selected = &module.value_capabilities[&(ty.clone(), capability)];
        if let Some(callee) = selected.callable {
            callees.insert(callee);
        } else {
            pending.extend(
                hew_sir::derived_capability_components(
                    &ty,
                    &module.aggregate_shapes,
                    &module.variant_shapes,
                )
                .expect("verified derived value capability"),
            );
        }
    }
    callees
}

/// Release reaches the selected consuming close, or the concrete fields of a
/// structural value. Erased callables and dynamic objects retain their runtime
/// release descriptor, so their callers must allow a continuation.
fn semantic_release_dependencies(
    module: &hew_sir::SemModule,
    ty: &hew_types::ResolvedTy,
    body: Option<CallableId>,
) -> (bool, BTreeSet<CallableId>) {
    use hew_types::{BuiltinType, ResolvedTy};
    let mut pending = vec![ty.clone()];
    let mut seen = BTreeSet::new();
    let mut callees = BTreeSet::new();
    let mut intrinsic = false;
    while let Some(ty) = pending.pop() {
        if !seen.insert(ty.clone()) {
            continue;
        }
        let own_record_close = matches!(module.resources.get(&ty),
            Some(hew_sir::ResourceRelease::RecordClose { close, .. }) if Some(*close) == body);
        if let Some(release) = module.resources.get(&ty).filter(|_| !own_record_close) {
            match release {
                hew_sir::ResourceRelease::RecordClose { close, .. }
                | hew_sir::ResourceRelease::OpaqueClose { close, .. } => {
                    callees.insert(*close);
                }
                hew_sir::ResourceRelease::Generator
                | hew_sir::ResourceRelease::ActorCall
                | hew_sir::ResourceRelease::ActorRequest
                | hew_sir::ResourceRelease::Stream
                | hew_sir::ResourceRelease::Sink => intrinsic = true,
                _ => {}
            }
            continue;
        }
        match &ty {
            ResolvedTy::Function { .. }
            | ResolvedTy::Closure { .. }
            | ResolvedTy::TraitObject { .. } => intrinsic = true,
            ResolvedTy::Tuple(fields) => pending.extend(fields.iter().cloned()),
            ResolvedTy::Array(element, size) if *size != 0 => pending.push((**element).clone()),
            ResolvedTy::Named {
                builtin:
                    Some(
                        BuiltinType::Vec
                        | BuiltinType::HashMap
                        | BuiltinType::HashSet
                        | BuiltinType::Rc,
                    ),
                args,
                ..
            } => pending.extend(args.iter().cloned()),
            _ => {
                if let Some(shape) = module.aggregate_shape_for_type(&ty) {
                    pending.extend(shape.fields.iter().map(|field| field.ty.clone()));
                }
                if let Some(shape) = module
                    .variant_shapes
                    .iter()
                    .find(|shape| shape.enum_ty == ty)
                {
                    pending.extend(
                        shape.variants.iter().flat_map(|variant| {
                            variant.fields.iter().map(|field| field.ty.clone())
                        }),
                    );
                }
            }
        }
    }
    (intrinsic, callees)
}

fn actor_release_types<'a>(
    actors: &'a [hew_sir::SemActor],
    operation: &'a hew_sir::ActorOperation,
) -> Vec<&'a hew_types::ResolvedTy> {
    match operation {
        hew_sir::ActorOperation::Spawn(id) => vec![&actors[id.0 as usize].state_ty],
        hew_sir::ActorOperation::Submit { message_ty, .. } => vec![message_ty],
        _ => Vec::new(),
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "one terminator dispatch closes direct and selected callback dependencies"
)]
pub(super) fn semantic_callables(checked: &hew_sir::CheckedModule<'_>) -> BTreeSet<CallableId> {
    let module = checked.module();
    let mut resumable = BTreeSet::new();
    let mut calls = BTreeMap::<_, Vec<_>>::new();
    for function in &module.functions {
        let mut types = BTreeMap::new();
        for parameter in &function.params {
            types.insert(parameter.value, parameter.ty.clone());
        }
        for block in &function.blocks {
            for parameter in &block.args {
                types.insert(parameter.value, parameter.ty.clone());
            }
            for operation in &block.ops {
                for result in &operation.results {
                    types.insert(result.id, result.ty.clone());
                }
            }
            block.terminator.visit_results(|result| {
                types.insert(result.id, result.ty.clone());
            });
        }
        let lifetimes = checked
            .function(function.callable)
            .expect("verified callable body")
            .place_lifetimes();
        for block in function
            .blocks
            .iter()
            .filter(|block| lifetimes.is_reachable(block.id))
        {
            for operation in &block.ops {
                let ty = match &operation.kind {
                    hew_sir::SemOpKind::DestroyValue { value } => Some(&types[&value.value]),
                    hew_sir::SemOpKind::StoreAssign { place, .. }
                    | hew_sir::SemOpKind::EndLifetime { place } => Some(
                        &function
                            .places
                            .iter()
                            .find(|candidate| candidate.id == *place)
                            .expect("verified place identity")
                            .ty,
                    ),
                    _ => None,
                };
                if let Some(ty) = ty {
                    let (intrinsic, dependencies) =
                        semantic_release_dependencies(module, ty, Some(function.callable));
                    if intrinsic {
                        resumable.insert(function.callable);
                    }
                    calls
                        .entry(function.callable)
                        .or_default()
                        .extend(dependencies);
                }
            }
            if let hew_sir::SemTerminator::ActorCall { operation, .. } = &block.terminator {
                if let hew_sir::ActorOperation::Spawn(actor) = operation {
                    let actor = &module.actors[actor.0 as usize];
                    calls
                        .entry(function.callable)
                        .or_default()
                        .extend(actor.init.iter().chain(&actor.start).copied());
                }
                for ty in actor_release_types(&module.actors, operation) {
                    let (intrinsic, dependencies) = semantic_release_dependencies(module, ty, None);
                    // Actor payload/state cleanup uses the checked continuation
                    // ABI even for a pure close, so a retained fault cannot
                    // unwind through the scheduler's synchronous drop callback.
                    if intrinsic || !dependencies.is_empty() {
                        resumable.insert(function.callable);
                    }
                    calls
                        .entry(function.callable)
                        .or_default()
                        .extend(dependencies);
                }
            }
            match &block.terminator {
                hew_sir::SemTerminator::Suspend {
                    kind: hew_sir::SuspendKind::StreamSend { park: false },
                    inputs,
                    ..
                } => {
                    let (intrinsic, dependencies) = semantic_release_dependencies(
                        module,
                        &types[&inputs[1].operand.value],
                        None,
                    );
                    if intrinsic {
                        resumable.insert(function.callable);
                    }
                    calls
                        .entry(function.callable)
                        .or_default()
                        .extend(dependencies);
                }
                hew_sir::SemTerminator::Suspend {
                    kind: hew_sir::SuspendKind::StreamNext { park: false },
                    ..
                } => {}
                hew_sir::SemTerminator::RecoverFault { .. }
                | hew_sir::SemTerminator::Suspend { .. }
                | hew_sir::SemTerminator::IndirectCall { .. } => {
                    // Callable values have no source-level non-suspension
                    // guarantee, so their invocation must admit suspension.
                    resumable.insert(function.callable);
                }
                hew_sir::SemTerminator::ActorCall {
                    operation: hew_sir::ActorOperation::Submit { policy, .. },
                    ..
                } if policy.may_suspend() => {
                    resumable.insert(function.callable);
                }
                hew_sir::SemTerminator::ActorCall {
                    operation:
                        hew_sir::ActorOperation::AwaitClosed(_)
                        | hew_sir::ActorOperation::SupervisorAwaitClosed(_)
                        | hew_sir::ActorOperation::SupervisorRoleAwaitClosed { .. }
                        | hew_sir::ActorOperation::StreamStart { .. }
                        | hew_sir::ActorOperation::CallStart(_),
                    ..
                } => {
                    resumable.insert(function.callable);
                }
                hew_sir::SemTerminator::RtCall {
                    family: hew_types::RuntimeCallFamily::StructuralFormat,
                    args,
                    ..
                } => {
                    let mut pending = vec![hew_sir::StructuralType::canonical(
                        &types[&args[0].operand.value],
                    )];
                    let mut seen = BTreeSet::new();
                    while let Some(key) = pending.pop() {
                        if !seen.insert(key.clone()) {
                            continue;
                        }
                        let selected = &module.structural_display[&key];
                        if let Some(callee) = selected.display {
                            calls.entry(function.callable).or_default().push(callee);
                        }
                        pending.extend(selected.members.iter().cloned());
                    }
                }
                hew_sir::SemTerminator::ValueCall { ty, capability, .. } => {
                    calls
                        .entry(function.callable)
                        .or_default()
                        .extend(semantic_value_callees(module, ty, *capability));
                }
                hew_sir::SemTerminator::WireCodec {
                    direction, plan, ..
                } if !direction.is_serialize() => {
                    plan.visit_types(&mut |ty| {
                        let (intrinsic, dependencies) =
                            semantic_release_dependencies(module, ty, None);
                        if intrinsic {
                            resumable.insert(function.callable);
                        }
                        calls
                            .entry(function.callable)
                            .or_default()
                            .extend(dependencies);
                    });
                    plan.visit_decode_capabilities(&mut |ty, capability| {
                        calls
                            .entry(function.callable)
                            .or_default()
                            .extend(semantic_value_callees(module, ty, capability));
                    });
                }
                hew_sir::SemTerminator::RtCall { family, args, .. } => {
                    if family.releases_receiver_contents() {
                        let (intrinsic, dependencies) = semantic_release_dependencies(
                            module,
                            &types[&args[0].operand.value],
                            None,
                        );
                        if intrinsic {
                            resumable.insert(function.callable);
                        }
                        calls
                            .entry(function.callable)
                            .or_default()
                            .extend(dependencies);
                    }
                    for capability in family.value_callback_capabilities() {
                        let receiver = &types[&args[0].operand.value];
                        let (_, arguments) =
                            hew_types::runtime_call::collection_type_arguments(receiver)
                                .expect("verified collection callback receiver");
                        calls
                            .entry(function.callable)
                            .or_default()
                            .extend(semantic_value_callees(module, &arguments[0], *capability));
                    }
                }
                hew_sir::SemTerminator::Call { callee, .. } => {
                    calls.entry(function.callable).or_default().push(*callee);
                }
                _ => {}
            }
        }
    }
    close_callers(resumable, &calls)
}

#[expect(
    clippy::too_many_lines,
    reason = "verify physical suspension against every direct and selected callback edge"
)]
pub(super) fn verify_callables(module: &PhysicalModule) -> Result<(), PhysicalError> {
    let mut resumable = BTreeSet::new();
    let mut calls = BTreeMap::<_, Vec<_>>::new();
    let releases = super::release::ReleaseEffects::compute(module);
    for function in &module.functions {
        for block in &function.blocks {
            for operation in &block.ops {
                let action = match operation {
                    super::PhysicalOp::Destroy { action, .. } => Some(*action),
                    super::PhysicalOp::Assign { destroy_old, .. } => *destroy_old,
                    super::PhysicalOp::StorageDead { destroy, .. } => *destroy,
                    _ => None,
                };
                if action.is_some_and(|action| releases.suspends(action)) {
                    resumable.insert(function.callable);
                }
            }
            if let PhysicalTerminator::ActorCall { operation, .. } = &block.terminator {
                if let hew_sir::ActorOperation::Spawn(actor) = operation {
                    let actor = &module.actors[actor.0 as usize];
                    calls
                        .entry(function.callable)
                        .or_default()
                        .extend(actor.init.iter().chain(&actor.start).copied());
                }
                for ty in actor_release_types(&module.actors, operation) {
                    if module
                        .actor_recipes
                        .get(ty)
                        .and_then(|recipe| recipe.destroy)
                        .is_some_and(|action| {
                            releases.suspends(action) || releases.raises_fault(action)
                        })
                    {
                        resumable.insert(function.callable);
                    }
                }
            }
            match &block.terminator {
                PhysicalTerminator::StreamSend {
                    park: false,
                    element,
                    ..
                } => {
                    if element
                        .destroy
                        .is_some_and(|action| releases.suspends(action))
                    {
                        resumable.insert(function.callable);
                    }
                }
                PhysicalTerminator::RecoverFault { .. }
                | PhysicalTerminator::NativeIo { .. }
                | PhysicalTerminator::Sleep { .. }
                | PhysicalTerminator::SleepUntil { .. }
                | PhysicalTerminator::TaskSelect { .. }
                | PhysicalTerminator::GeneratorYield { .. }
                | PhysicalTerminator::GeneratorNext { .. }
                | PhysicalTerminator::StreamNext { park: true, .. }
                | PhysicalTerminator::StreamSend { park: true, .. }
                | PhysicalTerminator::IndirectCall { .. }
                | PhysicalTerminator::TaskAwait { .. }
                | PhysicalTerminator::ActorAsk { .. }
                | PhysicalTerminator::TaskScopeJoin { .. }
                | PhysicalTerminator::ActorCall {
                    operation:
                        hew_sir::ActorOperation::AwaitClosed(_)
                        | hew_sir::ActorOperation::SupervisorAwaitClosed(_)
                        | hew_sir::ActorOperation::SupervisorRoleAwaitClosed { .. }
                        | hew_sir::ActorOperation::StreamStart { .. }
                        | hew_sir::ActorOperation::CallStart(_),
                    ..
                } => {
                    resumable.insert(function.callable);
                }
                PhysicalTerminator::ActorCall {
                    operation: hew_sir::ActorOperation::Submit { policy, .. },
                    ..
                } if policy.may_suspend() => {
                    resumable.insert(function.callable);
                }
                PhysicalTerminator::RuntimeCall { action, args, .. } => {
                    if super::runtime_receiver_release(module, action)?
                        .is_some_and(|action| releases.suspends(action))
                    {
                        resumable.insert(function.callable);
                    }
                    for capability in action.family.value_callback_capabilities() {
                        let argument = args.first().ok_or_else(|| {
                            PhysicalError::new("collection callback has no receiver operand")
                        })?;
                        let source = match *argument {
                            super::ArgumentTransfer::Borrow(source)
                            | super::ArgumentTransfer::BorrowMut(source)
                            | super::ArgumentTransfer::Move(source)
                            | super::ArgumentTransfer::Clone { source, .. } => source,
                        };
                        let receiver =
                            function.storage.get(source.0 as usize).ok_or_else(|| {
                                PhysicalError::new("collection callback has no receiver storage")
                            })?;
                        let (_, arguments) =
                            hew_types::runtime_call::collection_type_arguments(&receiver.ty)
                                .ok_or_else(|| {
                                    PhysicalError::new("collection callback has no receiver type")
                                })?;
                        calls.entry(function.callable).or_default().extend(
                            super::capability::callees(module, &arguments[0], *capability)?,
                        );
                    }
                    if let super::PhysicalRuntimeCarrier::StructuralFormat(glue) = action.carrier {
                        calls.entry(function.callable).or_default().extend(
                            super::structural::display_callees(&module.structural_glue, glue)?,
                        );
                    }
                }
                PhysicalTerminator::ValueCall { ty, capability, .. } => {
                    calls
                        .entry(function.callable)
                        .or_default()
                        .extend(super::capability::callees(module, ty, *capability)?);
                }
                PhysicalTerminator::WireCodec {
                    direction,
                    plan,
                    recipes,
                    ..
                } if !direction.is_serialize() => {
                    if recipes.values().any(|recipe| {
                        recipe
                            .destroy
                            .is_some_and(|action| releases.suspends(action))
                    }) {
                        resumable.insert(function.callable);
                    }
                    let mut dependencies = Vec::new();
                    plan.visit_decode_capabilities(&mut |ty, capability| {
                        dependencies.push(super::capability::callees(module, ty, capability));
                    });
                    for dependency in dependencies {
                        calls
                            .entry(function.callable)
                            .or_default()
                            .extend(dependency?);
                    }
                }
                PhysicalTerminator::Call { callee, .. } => {
                    calls.entry(function.callable).or_default().push(*callee);
                }
                _ => {}
            }
        }
    }
    let expected = close_callers(resumable, &calls);
    for glue in &module.structural_glue {
        let may_suspend = super::structural::display_callees(&module.structural_glue, glue.id)?
            .iter()
            .any(|callee| expected.contains(callee));
        if glue.is_resumable != may_suspend {
            return Err(PhysicalError::new(
                "structural rendering has an inconsistent resumable ABI",
            ));
        }
    }
    for ((ty, capability), selected) in &module.value_capabilities {
        let may_suspend = super::capability::callees(module, ty, *capability)?
            .iter()
            .any(|callee| expected.contains(callee));
        if may_suspend != selected.is_resumable {
            return Err(PhysicalError::new(
                "selected value callback has an inconsistent resumable ABI",
            ));
        }
    }
    for callable in &module.callables {
        if callable.is_resumable != expected.contains(&callable.id) {
            return Err(PhysicalError::new(format!(
                "callable {} (`{}`) has an inconsistent resumable ABI",
                callable.id.0, callable.symbol
            )));
        }
    }
    // Spawn invokes init and start through its caller's continuation,
    // terminal cleanup drives stop hooks as a resumable release, and the
    // system lane parks a suspending EXIT or DOWN hook like a handler. The
    // crash hook still runs through the runtime's synchronous callback ABI.
    for actor in &module.actors {
        if actor.crash.iter().any(|body| expected.contains(body)) {
            return Err(PhysicalError::new(
                "native actor crash hook suspension is not implemented",
            ));
        }
    }
    Ok(())
}
