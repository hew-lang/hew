//! Resumable callable ABI selection from explicit typed control flow.

use super::{CallableId, PhysicalError, PhysicalModule, PhysicalTerminator};
use std::collections::{BTreeMap, BTreeSet};

pub(super) fn verify_task_scopes(function: &super::PhysicalFunction) -> Result<(), PhysicalError> {
    use super::{PhysicalOp, TaskScopeId};
    let mut declared = BTreeSet::new();
    for op in function
        .blocks
        .iter()
        .flat_map(super::types::PhysicalBlock::operations)
    {
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
        for op in block.operations() {
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
                PhysicalOp::TaskSpawn { scope, .. } | PhysicalOp::TaskRace { scope, .. }
                    if !stack.contains(&(*scope, false)) =>
                {
                    return Err(PhysicalError::new(
                        "physical task spawn has no active lexical scope",
                    ));
                }
                PhysicalOp::TaskInvoke { scope, .. } if stack.last() != Some(&(*scope, false)) => {
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
    hew_sir::value_capability_callees(module, ty, capability)
        .expect("verified selected value capability")
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
            ResolvedTy::GenericCallable { .. }
            | ResolvedTy::Function { .. }
            | ResolvedTy::Closure { .. }
            | ResolvedTy::TraitObject { .. } => intrinsic = true,
            ResolvedTy::Tuple(fields) => pending.extend(fields.iter().cloned()),
            ResolvedTy::Array(element, size) if *size != 0 => pending.push((**element).clone()),
            ResolvedTy::Named {
                head:
                    hew_types::TypeHead::Builtin(
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
        hew_sir::ActorOperation::TimerSchedule { actor, message, .. } => actors[actor.0 as usize]
            .handlers
            .iter()
            .find(|handler| handler.message_id == *message)
            .map_or_else(Vec::new, |handler| handler.params.iter().collect()),
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
    let mut entry_polls = BTreeSet::new();
    let mut cyclic_bodies = BTreeSet::new();
    for function in &module.functions {
        if matches!(
            module.callables[function.callable.0 as usize].instance,
            hew_sir::CallableInstance::ActorConstruction { .. }
        ) {
            resumable.insert(function.callable);
        }
        let mut types = BTreeMap::new();
        for parameter in &function.params {
            types.insert(parameter.value, &parameter.ty);
        }
        for block in &function.blocks {
            for parameter in &block.args {
                types.insert(parameter.value, &parameter.ty);
            }
            for operation in &block.ops {
                for result in &operation.results {
                    types.insert(result.id, &result.ty);
                }
            }
            block.terminator.visit_results(|result| {
                types.insert(result.id, &result.ty);
            });
        }
        let certificate = checked
            .function(function.callable)
            .expect("verified callable body");
        let lifetimes = certificate.place_lifetimes();
        let cfg = certificate.cfg();
        let order: BTreeMap<_, _> = cfg
            .rpo()
            .iter()
            .enumerate()
            .map(|(i, id)| (*id, i))
            .collect();
        if cfg.rpo().iter().any(|block| {
            cfg.successors_of(*block).iter().any(|edge| {
                cfg.edge_target(*edge)
                    .is_some_and(|target| order[&target] <= order[block])
            })
        }) {
            cyclic_bodies.insert(function.callable);
        }
        let reenters_entry = !cfg.predecessors_of(function.entry).is_empty();
        for block in function
            .blocks
            .iter()
            .filter(|block| lifetimes.is_reachable(block.id))
        {
            for operation in block.operations() {
                // A scope's runtime handle lives in the resumable frame even
                // when `exit` leaves its drain unreachable.
                if matches!(operation.kind, hew_sir::SemOpKind::TaskScopeEnter { .. }) {
                    resumable.insert(function.callable);
                }
                let ty = match &operation.kind {
                    hew_sir::SemOpKind::DestroyValue { value } => Some(types[&value.value]),
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
                if matches!(
                    operation,
                    hew_sir::ActorOperation::SelfHandle(_)
                        | hew_sir::ActorOperation::Submit { .. }
                        | hew_sir::ActorOperation::TimerSchedule { .. }
                ) {
                    resumable.insert(function.callable);
                }
                if let hew_sir::ActorOperation::Spawn(actor) = operation {
                    let actor = &module.actors[actor.0 as usize];
                    calls
                        .entry(function.callable)
                        .or_default()
                        .extend(actor.init.iter().chain(&actor.start).copied());
                    if let Some(init) = actor.init {
                        for param in module.callables[init.0 as usize]
                            .signature
                            .params
                            .iter()
                            .skip(1)
                        {
                            let (intrinsic, dependencies) =
                                semantic_release_dependencies(module, &param.ty, None);
                            if intrinsic || !dependencies.is_empty() {
                                resumable.insert(function.callable);
                            }
                            calls
                                .entry(function.callable)
                                .or_default()
                                .extend(dependencies);
                        }
                    }
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
                    // A non-parking send can still yield while invoking an
                    // encoder. Its operation never waits for sink capacity.
                    resumable.insert(function.callable);
                    let (_, dependencies) = semantic_release_dependencies(
                        module,
                        types[&inputs[1].operand.value],
                        None,
                    );
                    calls
                        .entry(function.callable)
                        .or_default()
                        .extend(dependencies);
                }
                hew_sir::SemTerminator::Suspend {
                    kind: hew_sir::SuspendKind::StreamNext { park: false },
                    ..
                } => {}
                hew_sir::SemTerminator::IndirectCall {
                    targets, callee, ..
                } => {
                    if targets.may_be_unknown || callee.decision == hew_sir::BoundaryDecision::Move
                    {
                        resumable.insert(function.callable);
                    }
                    calls
                        .entry(function.callable)
                        .or_default()
                        .extend(&targets.known);
                }
                hew_sir::SemTerminator::Safepoint { .. }
                    if block.id == function.entry && !reenters_entry =>
                {
                    entry_polls.insert(function.callable);
                }
                hew_sir::SemTerminator::RecoverFault { .. }
                | hew_sir::SemTerminator::Safepoint { .. }
                | hew_sir::SemTerminator::Suspend { .. }
                | hew_sir::SemTerminator::ActorCall {
                    operation:
                        hew_sir::ActorOperation::SupervisorSpawn(_)
                        | hew_sir::ActorOperation::Submit { .. }
                        | hew_sir::ActorOperation::AwaitStopped(_)
                        | hew_sir::ActorOperation::LocalObservation {
                            kind:
                                hew_sir::LocalObservationKind::Link
                                | hew_sir::LocalObservationKind::Monitor,
                            ..
                        }
                        | hew_sir::ActorOperation::AwaitRestarted(_)
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
                        types[&args[0].operand.value],
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
                hew_sir::SemTerminator::WireCodec { codec, plan, .. } if !codec.is_serialize() => {
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
                    if *family == hew_types::RuntimeCallFamily::RcNew {
                        resumable.insert(function.callable);
                    }
                    if family.releases_receiver_contents() {
                        let (intrinsic, dependencies) = semantic_release_dependencies(
                            module,
                            types[&args[0].operand.value],
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
                        let receiver = types[&args[0].operand.value];
                        let (_, arguments) =
                            hew_types::runtime_call::collection_type_arguments(receiver)
                                .expect("verified collection callback receiver");
                        calls
                            .entry(function.callable)
                            .or_default()
                            .extend(semantic_value_callees(module, &arguments[0], *capability));
                    }
                }
                hew_sir::SemTerminator::DynCall { method, .. } => {
                    calls.entry(function.callable).or_default().extend(
                        module
                            .vtables
                            .iter()
                            .flat_map(|table| &table.slots)
                            .filter(|slot| slot.method == *method)
                            .map(|slot| slot.callee),
                    );
                }
                hew_sir::SemTerminator::Call { callee, .. } => {
                    calls.entry(function.callable).or_default().push(*callee);
                }
                _ => {}
            }
        }
    }
    // A one-shot entry poll need not create a continuation for finite code.
    // Close over the same call and release dependencies used for ABI selection;
    // recursive components never become bounded, and retain their entry polls.
    let bodies: BTreeSet<_> = module.functions.iter().map(|body| body.callable).collect();
    let mut bounded = BTreeSet::new();
    loop {
        let mut changed = false;
        for callable in &bodies {
            if !bounded.contains(callable)
                && !cyclic_bodies.contains(callable)
                && !resumable.contains(callable)
                && calls.get(callable).is_none_or(|callees| {
                    callees.iter().all(|callee| {
                        !resumable.contains(callee)
                            && (!bodies.contains(callee) || bounded.contains(callee))
                    })
                })
            {
                changed |= bounded.insert(*callable);
            }
        }
        if !changed {
            break;
        }
    }
    resumable.extend(entry_polls.difference(&bounded));
    close_callers(resumable, &calls)
}

pub(super) fn operation_suspends(
    releases: &super::ReleaseEffects,
    operation: &super::PhysicalOp,
) -> bool {
    let action = match operation {
        super::PhysicalOp::Destroy { action, .. } => Some(*action),
        super::PhysicalOp::Assign { destroy_old, .. } => *destroy_old,
        super::PhysicalOp::StorageDead { destroy, .. } => *destroy,
        _ => None,
    };
    action.is_some_and(|action| releases.suspends(action))
        || matches!(operation, super::PhysicalOp::TaskScopeEnter { .. })
}

#[expect(
    clippy::too_many_lines,
    reason = "one suspension authority for direct and selected callback edges"
)]
fn terminator_dependencies(
    module: &PhysicalModule,
    function: &super::PhysicalFunction,
    terminator: &PhysicalTerminator,
    releases: &super::ReleaseEffects,
) -> Result<(bool, Vec<CallableId>), PhysicalError> {
    let mut direct = false;
    let mut callees = Vec::new();
    if let PhysicalTerminator::ActorCall { operation, .. } = terminator {
        if matches!(
            operation,
            hew_sir::ActorOperation::SelfHandle(_)
                | hew_sir::ActorOperation::Submit { .. }
                | hew_sir::ActorOperation::TimerSchedule { .. }
        ) {
            direct = true;
        }
        if let hew_sir::ActorOperation::Spawn(actor) = operation {
            let actor = &module.actors[actor.0 as usize];
            callees.extend(actor.init.iter().chain(&actor.start).copied());
            if let Some(init) = actor.init {
                for param in module.callables[init.0 as usize].params.iter().skip(1) {
                    if module
                        .actor_recipes
                        .get(&param.ty)
                        .and_then(|recipe| recipe.destroy)
                        .is_some_and(|action| {
                            releases.suspends(action) || releases.raises_fault(action)
                        })
                    {
                        direct = true;
                    }
                }
            }
        }
        for ty in actor_release_types(&module.actors, operation) {
            if module
                .actor_recipes
                .get(ty)
                .and_then(|recipe| recipe.destroy)
                .is_some_and(|action| releases.suspends(action) || releases.raises_fault(action))
            {
                direct = true;
            }
        }
    }
    match terminator {
        PhysicalTerminator::RecoverFault { .. }
        | PhysicalTerminator::NativeIo { .. }
        | PhysicalTerminator::SemaphoreAcquire { .. }
        | PhysicalTerminator::ServiceAcquire { .. }
        | PhysicalTerminator::Offload { .. }
        | PhysicalTerminator::Safepoint { .. }
        | PhysicalTerminator::Sleep { .. }
        | PhysicalTerminator::SleepUntil { .. }
        | PhysicalTerminator::TaskSelect { .. }
        | PhysicalTerminator::GeneratorYield { .. }
        | PhysicalTerminator::GeneratorNext { .. }
        | PhysicalTerminator::StreamNext { park: true, .. }
        | PhysicalTerminator::StreamSend { .. }
        | PhysicalTerminator::TaskAwait { .. }
        | PhysicalTerminator::ActorAsk { .. }
        | PhysicalTerminator::TaskScopeJoin { .. }
        | PhysicalTerminator::ActorCall {
            operation:
                hew_sir::ActorOperation::SupervisorSpawn(_)
                | hew_sir::ActorOperation::Submit { .. }
                | hew_sir::ActorOperation::AwaitStopped(_)
                | hew_sir::ActorOperation::LocalObservation {
                    kind:
                        hew_sir::LocalObservationKind::Link | hew_sir::LocalObservationKind::Monitor,
                    ..
                }
                | hew_sir::ActorOperation::AwaitRestarted(_)
                | hew_sir::ActorOperation::StreamStart { .. }
                | hew_sir::ActorOperation::CallStart(_),
            ..
        } => {
            direct = true;
        }
        PhysicalTerminator::IndirectCall {
            targets, callee, ..
        } => {
            if targets.may_be_unknown || matches!(callee, super::ArgumentTransfer::Move(_)) {
                direct = true;
            }
            callees.extend(&targets.known);
        }
        PhysicalTerminator::RuntimeCall { action, args, .. } => {
            if action.family == hew_types::RuntimeCallFamily::RcNew {
                direct = true;
            }
            if super::runtime_receiver_release(module, action)?
                .is_some_and(|action| releases.suspends(action))
            {
                direct = true;
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
                let receiver = function.storage.get(source.0 as usize).ok_or_else(|| {
                    PhysicalError::new("collection callback has no receiver storage")
                })?;
                let (_, arguments) = hew_types::runtime_call::collection_type_arguments(
                    &receiver.ty,
                )
                .ok_or_else(|| PhysicalError::new("collection callback has no receiver type"))?;
                callees.extend(super::capability::callees(
                    module,
                    &arguments[0],
                    *capability,
                )?);
            }
            if let super::PhysicalRuntimeCarrier::StructuralFormat(glue) = action.carrier {
                callees.extend(super::structural::display_callees(
                    &module.structural_glue,
                    glue,
                )?);
            }
        }
        PhysicalTerminator::ValueCall { ty, capability, .. } => {
            callees.extend(super::capability::callees(module, ty, *capability)?);
        }
        PhysicalTerminator::WireCodec {
            codec,
            plan,
            recipes,
            ..
        } if !codec.is_serialize() => {
            if recipes.values().any(|recipe| {
                recipe
                    .destroy
                    .is_some_and(|action| releases.suspends(action))
            }) {
                direct = true;
            }
            let mut dependencies = Vec::new();
            plan.visit_decode_capabilities(&mut |ty, capability| {
                dependencies.push(super::capability::callees(module, ty, capability));
            });
            for dependency in dependencies {
                callees.extend(dependency?);
            }
        }
        PhysicalTerminator::DynCall { method, .. } => {
            callees.extend(
                module
                    .vtables
                    .iter()
                    .flat_map(|table| &table.slots)
                    .filter(|slot| slot.method == *method)
                    .map(|slot| slot.callee),
            );
        }
        PhysicalTerminator::Call { callee, .. } => {
            callees.push(*callee);
        }
        _ => {}
    }
    Ok((direct, callees))
}

pub(super) fn terminator_suspends(
    module: &PhysicalModule,
    function: &super::PhysicalFunction,
    terminator: &PhysicalTerminator,
) -> Result<bool, PhysicalError> {
    let (direct, callees) =
        terminator_dependencies(module, function, terminator, &module.releases)?;
    Ok(direct
        || callees
            .iter()
            .any(|id| module.callables[id.0 as usize].is_resumable))
}

pub(super) fn verify_callables(module: &PhysicalModule) -> Result<(), PhysicalError> {
    let mut resumable = BTreeSet::new();
    let mut calls = BTreeMap::<_, Vec<_>>::new();
    let releases = &module.releases;
    for function in &module.functions {
        if matches!(
            module.callables[function.callable.0 as usize].instance,
            hew_sir::CallableInstance::ActorConstruction { .. }
        ) {
            resumable.insert(function.callable);
        }
        for block in &function.blocks {
            if block
                .operations()
                .any(|op| operation_suspends(releases, op))
            {
                resumable.insert(function.callable);
            }
            let (direct, callees) =
                terminator_dependencies(module, function, &block.terminator, releases)?;
            if direct {
                resumable.insert(function.callable);
            }
            calls.entry(function.callable).or_default().extend(callees);
        }
    }
    let expected = close_callers(resumable, &calls);
    let resumable_methods: BTreeSet<_> = module
        .vtables
        .iter()
        .flat_map(|table| &table.slots)
        .filter(|slot| expected.contains(&slot.callee))
        .map(|slot| slot.method)
        .collect();
    for slot in module.vtables.iter().flat_map(|table| &table.slots) {
        if slot.is_resumable != resumable_methods.contains(&slot.method) {
            return Err(PhysicalError::new(
                "vtable slot has an inconsistent resumable ABI",
            ));
        }
    }
    for block in module
        .functions
        .iter()
        .flat_map(|function| &function.blocks)
    {
        if let PhysicalTerminator::DynCall {
            method,
            is_resumable,
            ..
        } = &block.terminator
        {
            if *is_resumable != resumable_methods.contains(method) {
                return Err(PhysicalError::new(
                    "dynamic dispatch has an inconsistent resumable ABI",
                ));
            }
        }
    }
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
        if callable.is_authored_close
            != module
                .resources
                .iter()
                .any(|resource| resource.release.close_callable() == Some(callable.id))
        {
            return Err(PhysicalError::new(
                "callable cleanup ancestry differs from its authored close lifecycle",
            ));
        }
        if callable.is_resumable != expected.contains(&callable.id) {
            return Err(PhysicalError::new(format!(
                "callable {} (`{}`) has an inconsistent resumable ABI",
                callable.id.0, callable.symbol
            )));
        }
    }
    Ok(())
}
