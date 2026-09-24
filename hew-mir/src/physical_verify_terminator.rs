//! Independent verification of terminator successors and control-flow edges.

use super::{
    actor_signature, aggregate_glue, apply_edge, call_successors, call_successors_with_handback,
    callable, callable_for, consume_if_owned, defer, define, generators, initialized, partial,
    runtime_receiver_release, semantic_type_facts, storage, variant_glue, vector_glue,
    verify_map_call, verify_set_call, verify_value_recipe, verify_vector_call, wire,
    ArgumentTransfer, BTreeSet, BlockId, BorrowDependents, BuiltinType, CloneAction, FaultState,
    FlowState, InitState, OwnKind, PhysicalEdge, PhysicalError, PhysicalFunction, PhysicalModule,
    PhysicalRuntimeCarrier, PhysicalSelectSource, PhysicalTerminator, PhysicalValueRecipe,
    ResolvedTy, ReturnTransfer, RuntimeArgumentEffect, RuntimeCallFamily, RuntimeResultEffect,
    SemParamPassing, StorageId, StorageOrigin, ValueCapability,
};

#[allow(
    clippy::too_many_lines,
    reason = "the terminator transfer is the complete status/result/fault initialization contract"
)]
pub(crate) fn terminator_successors(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    borrows: &BorrowDependents,
    terminator: &PhysicalTerminator,
    mut state: FlowState,
    block: BlockId,
    defer_plan: &defer::Plan,
) -> Result<Vec<(BlockId, FlowState)>, PhysicalError> {
    if matches!(
        terminator,
        PhysicalTerminator::EnterDefer { .. }
            | PhysicalTerminator::FinishDefer { .. }
            | PhysicalTerminator::CleanupDispatch { .. }
            | PhysicalTerminator::CheckedRaiseFault { .. }
    ) {
        return defer::successors(function, borrows, defer_plan, terminator, state, block);
    }
    if matches!(
        terminator,
        PhysicalTerminator::Return { .. }
            | PhysicalTerminator::PropagateFault { .. }
            | PhysicalTerminator::Trap(_)
            | PhysicalTerminator::Unreachable
    ) && (!state.defers.pending.is_empty() || !state.defers.active.is_empty())
    {
        return Err(PhysicalError::new(
            "physical exit leaves pending actions or live fault parks",
        ));
    }
    if matches!(
        terminator,
        PhysicalTerminator::Return { .. }
            | PhysicalTerminator::Trap(_)
            | PhysicalTerminator::Unreachable
            | PhysicalTerminator::PropagateFault { .. }
    ) && function.storage.iter().any(|slot| {
        slot.borrow_parent.is_some() && state.slots[slot.id.0 as usize] != InitState::Uninitialized
    }) {
        return Err(PhysicalError::new(
            "physical function exit leaves a local loan live",
        ));
    }
    if matches!(
        terminator,
        PhysicalTerminator::Return { .. }
            | PhysicalTerminator::Trap(_)
            | PhysicalTerminator::Unreachable
            | PhysicalTerminator::PropagateFault { .. }
    ) && state
        .active
        .iter()
        .any(|active| *active != InitState::Uninitialized)
    {
        return Err(PhysicalError::new(
            "physical function exit leaves local storage active",
        ));
    }
    match terminator {
        PhysicalTerminator::RemoteAsk {
            target,
            payload,
            timeout,
            result,
            normal,
            cancel,
            unwind,
            ..
        } => {
            // The pid is borrowed, the message is consumed into its encoding
            // before the caller parks and the timeout is a copied scalar.
            for source in [target, payload, timeout] {
                initialized(function, &state, *source, block, "remote ask request")?;
            }
            consume_if_owned(function, borrows, &mut state, *payload)?;
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(
                    "remote ask cannot replace an active fault",
                ));
            }
            let mut completed = state.clone();
            define(
                function,
                borrows,
                &mut completed,
                *result,
                block,
                "remote ask result",
            )?;
            let mut successors = vec![apply_edge(function, borrows, normal, completed, block)?];
            state.fault = FaultState::Active;
            let mut cancelled = state.clone();
            cancelled.exit = defer::CANCEL;
            successors.push(apply_edge(function, borrows, cancel, cancelled, block)?);
            state.exit = defer::TRAP;
            successors.push(apply_edge(function, borrows, unwind, state, block)?);
            Ok(successors)
        }
        PhysicalTerminator::ActorAsk {
            args,
            result,
            normal,
            cancel,
            unwind,
            ..
        } => {
            // The target is borrowed to address the actor; the request
            // arguments are consumed into the message wrapper.
            for argument in args {
                let (ArgumentTransfer::Borrow(source) | ArgumentTransfer::Move(source)) = argument
                else {
                    return Err(PhysicalError::new("ask must consume its complete request"));
                };
                initialized(function, &state, *source, block, "ask request")?;
                if matches!(argument, ArgumentTransfer::Move(_)) {
                    consume_if_owned(function, borrows, &mut state, *source)?;
                }
            }
            if state.fault != FaultState::None {
                return Err(PhysicalError::new("ask cannot replace an active fault"));
            }
            let mut completed = state.clone();
            define(
                function,
                borrows,
                &mut completed,
                *result,
                block,
                "ask result",
            )?;
            completed.fault = FaultState::MaybeActive;
            let mut successors = vec![apply_edge(function, borrows, normal, completed, block)?];
            state.fault = FaultState::Active;
            let mut cancelled = state.clone();
            cancelled.exit = defer::CANCEL;
            successors.push(apply_edge(function, borrows, cancel, cancelled, block)?);
            state.exit = defer::TRAP;
            successors.push(apply_edge(function, borrows, unwind, state, block)?);
            Ok(successors)
        }
        PhysicalTerminator::TaskSelect {
            sources,
            timeout,
            result,
            normal,
            cancel,
            unwind,
            ..
        } => {
            for source in sources {
                let ArgumentTransfer::Borrow(handle) = source.transfer() else {
                    return Err(PhysicalError::new(
                        "selection must borrow its source handles",
                    ));
                };
                initialized(function, &state, handle, block, "selected source")?;
            }
            if let Some(timeout) = timeout {
                initialized(function, &state, *timeout, block, "selection timeout")?;
            }
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(
                    "selection cannot replace an active fault",
                ));
            }
            let mut completed = state.clone();
            define(
                function,
                borrows,
                &mut completed,
                *result,
                block,
                "selected source index",
            )?;
            let mut successors = vec![apply_edge(function, borrows, normal, completed, block)?];
            state.fault = FaultState::Active;
            let mut cancelled = state.clone();
            cancelled.exit = defer::CANCEL;
            successors.push(apply_edge(function, borrows, cancel, cancelled, block)?);
            state.exit = defer::TRAP;
            successors.push(apply_edge(function, borrows, unwind, state, block)?);
            Ok(successors)
        }
        PhysicalTerminator::GeneratorYield { .. } | PhysicalTerminator::GeneratorNext { .. } => {
            generators::successors(function, borrows, terminator, state, block)
        }
        PhysicalTerminator::StreamNext {
            stream,
            result,
            normal,
            cancel,
            unwind,
            ..
        } => {
            let ArgumentTransfer::BorrowMut(stream) = stream else {
                return Err(PhysicalError::new(
                    "stream receive requires an exclusive stream",
                ));
            };
            initialized(function, &state, *stream, block, "received stream")?;
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(
                    "stream receive cannot replace an active fault",
                ));
            }
            let mut completed = state.clone();
            define(
                function,
                borrows,
                &mut completed,
                *result,
                block,
                "received element",
            )?;
            let mut successors = vec![apply_edge(function, borrows, normal, completed, block)?];
            state.fault = FaultState::Active;
            let mut cancelled = state.clone();
            cancelled.exit = defer::CANCEL;
            successors.push(apply_edge(function, borrows, cancel, cancelled, block)?);
            state.exit = defer::TRAP;
            successors.push(apply_edge(function, borrows, unwind, state, block)?);
            Ok(successors)
        }
        PhysicalTerminator::StreamSend {
            sink,
            value,
            normal,
            closed,
            full,
            cancel,
            unwind,
            ..
        } => {
            let (ArgumentTransfer::Borrow(sink), ArgumentTransfer::Move(value)) = (sink, value)
            else {
                return Err(PhysicalError::new(
                    "stream send borrows its sink and consumes its element",
                ));
            };
            initialized(function, &state, *sink, block, "sending sink")?;
            initialized(function, &state, *value, block, "sent element")?;
            consume_if_owned(function, borrows, &mut state, *value)?;
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(
                    "stream send cannot replace an active fault",
                ));
            }
            let mut successors = vec![
                apply_edge(function, borrows, normal, state.clone(), block)?,
                apply_edge(function, borrows, closed, state.clone(), block)?,
            ];
            if let Some(full) = full {
                successors.push(apply_edge(function, borrows, full, state.clone(), block)?);
            }
            state.fault = FaultState::Active;
            let mut cancelled = state.clone();
            cancelled.exit = defer::CANCEL;
            successors.push(apply_edge(function, borrows, cancel, cancelled, block)?);
            state.exit = defer::TRAP;
            successors.push(apply_edge(function, borrows, unwind, state, block)?);
            Ok(successors)
        }
        PhysicalTerminator::TaskAwait {
            task,
            result,
            normal,
            cancel,
            unwind,
        } => {
            let ArgumentTransfer::Move(task) = task else {
                return Err(PhysicalError::new("task await must consume its handle"));
            };
            initialized(function, &state, *task, block, "await task")?;
            consume_if_owned(function, borrows, &mut state, *task)?;
            if state.fault != FaultState::None {
                return Err(PhysicalError::new("await cannot replace an active fault"));
            }
            let mut completed = state.clone();
            if let Some(result) = result {
                define(
                    function,
                    borrows,
                    &mut completed,
                    *result,
                    block,
                    "await result",
                )?;
            }
            let mut successors = normal
                .as_ref()
                .map(|normal| apply_edge(function, borrows, normal, completed, block))
                .transpose()?
                .into_iter()
                .collect::<Vec<_>>();
            state.fault = FaultState::Active;
            let mut cancelled = state.clone();
            cancelled.exit = defer::CANCEL;
            successors.push(apply_edge(function, borrows, cancel, cancelled, block)?);
            state.exit = defer::TRAP;
            successors.push(apply_edge(function, borrows, unwind, state, block)?);
            Ok(successors)
        }
        PhysicalTerminator::TaskScopeJoin {
            mode,
            normal,
            unwind,
            ..
        } => {
            if mode.preserves_fault() {
                if state.fault != FaultState::Active {
                    return Err(PhysicalError::new(
                        "fault drain requires an active primary fault",
                    ));
                }
                Ok(vec![
                    apply_edge(function, borrows, normal, state.clone(), block)?,
                    apply_edge(function, borrows, unwind, state, block)?,
                ])
            } else {
                if state.fault != FaultState::None {
                    return Err(PhysicalError::new(
                        "normal drain cannot replace an active fault",
                    ));
                }
                let completed = apply_edge(function, borrows, normal, state.clone(), block)?;
                state.fault = FaultState::Active;
                state.exit = defer::TRAP | defer::CANCEL;
                Ok(vec![
                    completed,
                    apply_edge(function, borrows, unwind, state, block)?,
                ])
            }
        }
        PhysicalTerminator::NativeIo {
            args,
            result,
            normal,
            cancel,
            unwind,
            ..
        } => {
            let mut successors = call_successors(
                function,
                borrows,
                args,
                Some(*result),
                Some(normal),
                Some(unwind),
                state.clone(),
                block,
            )?;
            let (_, mut cancelled) = call_successors(
                function,
                borrows,
                args,
                Some(*result),
                Some(normal),
                Some(cancel),
                state,
                block,
            )?
            .pop()
            .expect("native I/O cancel successor");
            cancelled.exit = defer::CANCEL;
            successors.push((cancel.target, cancelled));
            Ok(successors)
        }
        PhysicalTerminator::Sleep {
            duration: operand,
            normal,
            cancel,
            unwind,
        }
        | PhysicalTerminator::SleepUntil {
            deadline: operand,
            normal,
            cancel,
            unwind,
        } => {
            initialized(function, &state, *operand, block, "sleep input")?;
            if state.fault != FaultState::None {
                return Err(PhysicalError::new("sleep cannot overwrite an active fault"));
            }
            let mut successors = vec![apply_edge(function, borrows, normal, state.clone(), block)?];
            state.fault = FaultState::Active;
            let mut cancelled = state.clone();
            cancelled.exit = defer::CANCEL;
            successors.push(apply_edge(function, borrows, cancel, cancelled, block)?);
            state.exit = defer::TRAP;
            successors.push(apply_edge(function, borrows, unwind, state, block)?);
            Ok(successors)
        }
        PhysicalTerminator::EnterDefer { .. }
        | PhysicalTerminator::FinishDefer { .. }
        | PhysicalTerminator::CleanupDispatch { .. }
        | PhysicalTerminator::CheckedRaiseFault { .. } => {
            unreachable!("defer boundary handled above")
        }
        PhysicalTerminator::RecoverFault {
            result,
            normal,
            unwind,
            ..
        } => {
            if state.fault != FaultState::Active {
                return Err(PhysicalError::new(
                    "scope recovery requires an active fault",
                ));
            }
            let mut recovered = state.clone();
            recovered.fault = FaultState::None;
            recovered.exit = defer::ORDINARY;
            define(
                function,
                borrows,
                &mut recovered,
                *result,
                block,
                "scope failure",
            )?;
            Ok(vec![
                apply_edge(function, borrows, normal, recovered, block)?,
                apply_edge(function, borrows, unwind, state, block)?,
            ])
        }
        PhysicalTerminator::IndirectCall {
            callee: receiver,
            args,
            result,
            normal,
            unwind,
            ..
        }
        | PhysicalTerminator::DynCall {
            receiver,
            args,
            result,
            normal,
            unwind,
            ..
        } => {
            let transfers = std::iter::once(*receiver)
                .chain(args.iter().copied())
                .collect::<Vec<_>>();
            call_successors(
                function,
                borrows,
                &transfers,
                *result,
                normal.as_ref(),
                unwind.as_ref(),
                state,
                block,
            )
        }

        PhysicalTerminator::Return { value } => {
            callable::verify_capture_return(function, borrows, &state)?;
            if state.exit != defer::ORDINARY {
                return Err(PhysicalError::new(
                    "physical trap cleanup cannot return normally",
                ));
            }
            if function.storage.iter().any(|slot| {
                matches!(
                    slot.origin,
                    StorageOrigin::ActorState {
                        initialized: false,
                        ..
                    }
                ) && state.slots[slot.id.0 as usize] != InitState::Initialized
            }) {
                return Err(PhysicalError::new(format!(
                    "physical bb{} returns from init with a deferred actor field uninitialized",
                    block.0
                )));
            }
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(format!(
                    "physical bb{} returns normally while owning an active fault",
                    block.0
                )));
            }
            if let Some(value) = value {
                let id = match value {
                    ReturnTransfer::Borrow(id)
                    | ReturnTransfer::Move(id)
                    | ReturnTransfer::Clone { source: id, .. } => *id,
                };
                initialized(function, &state, id, block, "return")?;
            }
            Ok(vec![])
        }
        PhysicalTerminator::Goto(edge) => {
            Ok(vec![apply_edge(function, borrows, edge, state, block)?])
        }
        PhysicalTerminator::Branch {
            condition,
            then_target,
            else_target,
        } => {
            initialized(function, &state, *condition, block, "branch")?;
            Ok(vec![
                apply_edge(function, borrows, then_target, state.clone(), block)?,
                apply_edge(function, borrows, else_target, state, block)?,
            ])
        }
        PhysicalTerminator::SwitchVariant {
            scrutinee, arms, ..
        } => {
            initialized(function, &state, *scrutinee, block, "variant switch")?;
            let mut successors = Vec::with_capacity(arms.len());
            for arm in arms {
                let mut arm_state = state.clone();
                consume_if_owned(function, borrows, &mut arm_state, *scrutinee)?;
                for field in &arm.fields {
                    define(
                        function,
                        borrows,
                        &mut arm_state,
                        *field,
                        block,
                        "variant payload",
                    )?;
                }
                successors.push(apply_edge(
                    function,
                    borrows,
                    &arm.target,
                    arm_state,
                    block,
                )?);
            }
            Ok(successors)
        }
        PhysicalTerminator::CheckedBinary {
            lhs,
            rhs,
            result,
            normal,
            failures,
            ..
        } => {
            initialized(function, &state, *lhs, block, "checked binary operation")?;
            initialized(function, &state, *rhs, block, "checked binary operation")?;

            let mut normal_state = state.clone();
            define(
                function,
                borrows,
                &mut normal_state,
                *result,
                block,
                "checked binary result",
            )?;
            let mut successors = vec![apply_edge(function, borrows, normal, normal_state, block)?];
            for failure in failures {
                let mut failed = state.clone();
                failed.exit = defer::TRAP;
                successors.push(apply_edge(function, borrows, &failure.edge, failed, block)?);
            }
            Ok(successors)
        }
        PhysicalTerminator::Call {
            args,
            result,
            normal,
            unwind,
            handback,
            ..
        } => call_successors_with_handback(
            function,
            borrows,
            args,
            *result,
            *handback,
            normal.as_ref(),
            unwind.as_ref(),
            state,
            block,
        ),
        PhysicalTerminator::ActorCall {
            operation,
            args,
            result,
            normal,
            unwind,
            ..
        } => {
            let mut successors = call_successors(
                function,
                borrows,
                args,
                *result,
                Some(normal),
                unwind.as_ref(),
                state,
                block,
            )?;
            if operation.retains_cleanup_fault() {
                successors[0].1.fault = FaultState::MaybeActive;
            }
            Ok(successors)
        }
        PhysicalTerminator::WireCodec {
            input,
            result,
            normal,
            unwind,
            ..
        } => call_successors(
            function,
            borrows,
            std::slice::from_ref(input),
            Some(*result),
            Some(normal),
            Some(unwind),
            state,
            block,
        ),
        PhysicalTerminator::ValueCall {
            args,
            result,
            normal,
            unwind,
            ..
        } => call_successors(
            function,
            borrows,
            args,
            Some(*result),
            Some(normal),
            Some(unwind),
            state,
            block,
        ),
        PhysicalTerminator::ExternCall {
            args,
            result,
            normal,
            ..
        } => call_successors(
            function,
            borrows,
            args,
            *result,
            Some(normal),
            None,
            state,
            block,
        ),
        PhysicalTerminator::RuntimeCall {
            action,
            args,
            result,
            normal,
            failure,
            ..
        } => {
            let failure_inputs = action
                .family
                .semantic_contract()
                .is_some_and(hew_types::RuntimeSemanticContract::preserves_inputs_on_failure)
                .then(|| state.clone());
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(format!(
                    "physical bb{} issues a runtime call while an earlier fault is active",
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
                initialized(function, &state, source, block, "runtime call argument")?;
                if storage(function, source)?.own == OwnKind::Guaranteed
                    && !matches!(
                        argument,
                        ArgumentTransfer::Borrow(_) | ArgumentTransfer::BorrowMut(_)
                    )
                {
                    return Err(PhysicalError::new(
                        "physical guaranteed runtime argument must use its borrow contract",
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
                    *result,
                    block,
                    "runtime call result",
                )?;
            }
            // The call can release contents, so from its normal edge the
            // frame may own a fault and SIR owes the cleanup dispatch.
            if runtime_receiver_release(module, action)?
                .is_some_and(|release| module.releases.raises_fault(release))
            {
                normal_state.fault = FaultState::MaybeActive;
            }
            // A never-returning action ends the path; its normal edge is only
            // the structural unreachable continuation.
            let returns = !action.family.semantic_contract().is_some_and(|contract| {
                matches!(contract.result, hew_types::RuntimeResultEffect::Never)
            });
            let mut successors = Vec::new();
            if returns {
                successors.push(apply_edge(function, borrows, normal, normal_state, block)?);
            }
            if let Some(failure) = failure {
                if let Some(preserved) = failure_inputs {
                    state = preserved;
                }
                state.exit = defer::TRAP;
                if action
                    .family
                    .semantic_contract()
                    .is_some_and(hew_types::RuntimeSemanticContract::propagates_fault)
                {
                    state.fault = FaultState::Active;
                }
                if let Some(result) = result {
                    state.slots[result.0 as usize] = InitState::Uninitialized;
                }
                successors.push(apply_edge(function, borrows, failure, state, block)?);
            }
            Ok(successors)
        }
        PhysicalTerminator::Panic { message, cleanup } => {
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(
                    "physical panic cannot overwrite an active fault",
                ));
            }
            let ArgumentTransfer::Borrow(source) = message else {
                return Err(PhysicalError::new("physical panic must borrow its message"));
            };
            initialized(function, &state, *source, block, "panic message")?;
            state.fault = FaultState::Active;
            state.exit = defer::TRAP;
            Ok(vec![apply_edge(function, borrows, cleanup, state, block)?])
        }
        PhysicalTerminator::Trap(_) => {
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(format!(
                    "physical bb{} creates a trap while an earlier fault is active",
                    block.0
                )));
            }
            Ok(vec![])
        }
        PhysicalTerminator::Unreachable => {
            if state.fault != FaultState::None {
                return Err(PhysicalError::new(format!(
                    "physical bb{} abandons an active fault at unreachable",
                    block.0
                )));
            }
            Ok(vec![])
        }
        PhysicalTerminator::PropagateFault { handback } => {
            if state.fault != FaultState::Active {
                return Err(PhysicalError::new(format!(
                    "physical bb{} propagates a fault that is not initialized",
                    block.0
                )));
            }
            if let Some(handback) = handback {
                initialized(function, &state, *handback, block, "receiver handback")?;
                consume_if_owned(function, borrows, &mut state, *handback)?;
            }
            if function.storage.iter().any(|slot| {
                slot.own == OwnKind::Owned
                    && !matches!(
                        slot.origin,
                        StorageOrigin::Capture { .. }
                            | StorageOrigin::ActorState {
                                initialized: true,
                                ..
                            }
                    )
                    && function
                        .place_storage
                        .get(&slot.id)
                        .is_none_or(|place| place.root == slot.id)
                    && state.slots[slot.id.0 as usize] != InitState::Uninitialized
            }) {
                return Err(PhysicalError::new(
                    "physical fault propagation leaves owned storage initialized",
                ));
            }
            Ok(vec![])
        }
    }
}

pub(crate) fn merge_flow(existing: &mut FlowState, incoming: &FlowState) -> bool {
    let mut changed = existing.defers.join(&incoming.defers);
    let exit = existing.exit | incoming.exit;
    changed |= exit != existing.exit;
    existing.exit = exit;
    for (left, right) in existing
        .slots
        .iter_mut()
        .zip(&incoming.slots)
        .chain(existing.active.iter_mut().zip(&incoming.active))
    {
        let merged = if *left == *right {
            *left
        } else {
            InitState::MaybeInitialized
        };
        if *left != merged {
            *left = merged;
            changed = true;
        }
    }
    let merged_fault = if existing.fault == incoming.fault {
        existing.fault
    } else {
        FaultState::MaybeActive
    };
    if existing.fault != merged_fault {
        existing.fault = merged_fault;
        changed = true;
    }
    changed
}

#[allow(
    clippy::too_many_lines,
    reason = "one exhaustive verifier match keeps every physical terminator contract visible together"
)]
pub(crate) fn verify_terminator(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    blocks: &BTreeSet<BlockId>,
    terminator: &PhysicalTerminator,
) -> Result<(), PhysicalError> {
    let slot = |id: StorageId| {
        function
            .storage
            .get(id.0 as usize)
            .filter(|storage| storage.id == id)
            .ok_or_else(|| PhysicalError::new(format!("unknown physical storage {}", id.0)))
    };
    let edge = |edge: &PhysicalEdge| {
        partial::verify_edge(function, edge)?;
        if blocks.contains(&edge.target) {
            for (source, destination) in &edge.transfers {
                // A loan may be renamed across an edge, but only onto a
                // parameter that names the same owner: the transfer copies the
                // alias, never an obligation.
                let loan_rename = slot(*source)?.own == OwnKind::Guaranteed
                    && slot(*source)?.borrow_parent.is_some()
                    && slot(*source)?.borrow_parent == slot(*destination)?.borrow_parent;
                if slot(*source)?.ty != slot(*destination)?.ty
                    || slot(*source)?.own != slot(*destination)?.own
                    || (slot(*source)?.own == OwnKind::Guaranteed && !loan_rename)
                {
                    return Err(PhysicalError::new(format!(
                        "physical edge to block {} transfers incompatible storage",
                        edge.target.0
                    )));
                }
            }
            Ok(())
        } else {
            Err(PhysicalError::new(format!(
                "physical edge targets unknown block {}",
                edge.target.0
            )))
        }
    };
    match terminator {
        PhysicalTerminator::RemoteAsk {
            actor,
            message,
            target,
            payload,
            timeout,
            result,
            normal,
            cancel,
            unwind,
        } => {
            let signature = module
                .actors
                .get(actor.0 as usize)
                .filter(|descriptor| descriptor.id == *actor)
                .ok_or_else(|| PhysicalError::new("remote ask requires its exact actor"))?
                .remote_signature(
                    *message,
                    &slot(*target)?.ty,
                    slot(*result)?.ty.clone(),
                    true,
                )
                .map_err(PhysicalError::new)?;
            let types = [&slot(*target)?.ty, &slot(*payload)?.ty, &slot(*timeout)?.ty];
            if signature.params.len() != types.len()
                || signature
                    .params
                    .iter()
                    .zip(types)
                    .any(|(parameter, ty)| parameter.ty != *ty)
            {
                return Err(PhysicalError::new(
                    "remote ask differs from its member's pid, message and timeout",
                ));
            }
            edge(normal)?;
            edge(cancel)?;
            edge(unwind)
        }
        PhysicalTerminator::ActorAsk {
            actor,
            message,
            sealed,
            args,
            result,
            normal,
            cancel,
            unwind,
            ..
        } => {
            let target = match args.first() {
                Some(ArgumentTransfer::Borrow(source)) => slot(*source)?.ty.clone(),
                _ => return Err(PhysicalError::new("ask must borrow its target first")),
            };
            let signature = module
                .actors
                .get(actor.0 as usize)
                .filter(|descriptor| descriptor.id == *actor)
                .ok_or_else(|| PhysicalError::new("ask requires its exact actor descriptor"))?
                .ask_signature(
                    &module.defs,
                    *message,
                    &target,
                    slot(*result)?.ty.clone(),
                    *sealed,
                )
                .map_err(PhysicalError::new)?;
            if args.len() != signature.params.len() || slot(*result)?.ty != signature.return_ty {
                return Err(PhysicalError::new(
                    "ask differs from its full protocol signature",
                ));
            }
            for (index, (argument, parameter)) in args.iter().zip(&signature.params).enumerate() {
                let (ArgumentTransfer::Borrow(source) | ArgumentTransfer::Move(source)) = argument
                else {
                    return Err(PhysicalError::new("ask must transfer its complete request"));
                };
                let borrowed = matches!(argument, ArgumentTransfer::Borrow(_));
                if borrowed != (index == 0) {
                    return Err(PhysicalError::new(
                        "ask borrows its target and transfers its complete request",
                    ));
                }
                if slot(*source)?.ty != parameter.ty {
                    return Err(PhysicalError::new(
                        "ask request field changes its protocol type",
                    ));
                }
            }
            edge(normal)?;
            edge(cancel)?;
            edge(unwind)
        }
        PhysicalTerminator::TaskSelect {
            sources,
            timeout,
            result,
            normal,
            cancel,
            unwind,
            ..
        } => {
            if sources.is_empty() && timeout.is_none() {
                return Err(PhysicalError::new("selection requires a source or timeout"));
            }
            for source in sources {
                let ArgumentTransfer::Borrow(handle) = source.transfer() else {
                    return Err(PhysicalError::new(
                        "selection requires borrowed source handles",
                    ));
                };
                let ty = &slot(handle)?.ty;
                let agrees = match source {
                    PhysicalSelectSource::Task(_) => matches!(ty, ResolvedTy::Task(_)),
                    PhysicalSelectSource::ActorCall(_) => {
                        ty.is_builtin(hew_types::BuiltinType::ActorCall)
                    }
                    PhysicalSelectSource::StreamNext(_) => {
                        ty.is_builtin(hew_types::BuiltinType::Stream)
                    }
                };
                if !agrees {
                    return Err(PhysicalError::new(
                        "selection source type disagrees with its registration",
                    ));
                }
            }
            if let Some(timeout) = timeout {
                if slot(*timeout)?.ty != ResolvedTy::Duration
                    || slot(*timeout)?.own != OwnKind::None
                {
                    return Err(PhysicalError::new(
                        "selection timeout must be a trivial duration",
                    ));
                }
            }
            if slot(*result)?.ty != ResolvedTy::I64 || slot(*result)?.own != OwnKind::None {
                return Err(PhysicalError::new(
                    "selection result must be a trivial source index",
                ));
            }
            edge(normal)?;
            edge(cancel)?;
            edge(unwind)
        }
        PhysicalTerminator::GeneratorYield { .. } | PhysicalTerminator::GeneratorNext { .. } => {
            generators::verify_suspend(module, function, terminator)?;
            for successor in defer::edges(terminator) {
                edge(successor)?;
            }
            Ok(())
        }
        PhysicalTerminator::StreamNext {
            stream,
            element: recipe,
            result,
            ..
        } => {
            let ArgumentTransfer::BorrowMut(stream) = stream else {
                return Err(PhysicalError::new(
                    "stream receive requires an exclusive stream",
                ));
            };
            let element = hew_sir::stream_element(&slot(*stream)?.ty)
                .ok_or_else(|| PhysicalError::new("stream receive has no stream input"))?;
            if recipe.ty != *element
                || slot(*result)?.ty
                    != ResolvedTy::named_builtin(
                        "Option",
                        BuiltinType::Option,
                        vec![element.clone()],
                    )
            {
                return Err(PhysicalError::new(
                    "stream receive changes its element type",
                ));
            }
            verify_value_recipe(module, recipe)?;
            for successor in defer::edges(terminator) {
                edge(successor)?;
            }
            Ok(())
        }
        PhysicalTerminator::StreamSend {
            sink,
            value,
            element,
            ..
        } => {
            let (ArgumentTransfer::Borrow(sink), ArgumentTransfer::Move(value)) = (sink, value)
            else {
                return Err(PhysicalError::new(
                    "stream send borrows its sink and consumes its element",
                ));
            };
            if hew_sir::sink_element(&slot(*sink)?.ty) != Some(&element.ty)
                || slot(*value)?.ty != element.ty
            {
                return Err(PhysicalError::new(
                    "stream send element differs from its sink",
                ));
            }
            verify_value_recipe(module, element)?;
            for successor in defer::edges(terminator) {
                edge(successor)?;
            }
            Ok(())
        }
        PhysicalTerminator::TaskAwait {
            task,
            result,
            normal,
            cancel,
            unwind,
        } => {
            let ArgumentTransfer::Move(task) = task else {
                return Err(PhysicalError::new("await requires a moved task handle"));
            };
            let ResolvedTy::Task(output) = &slot(*task)?.ty else {
                return Err(PhysicalError::new("await requires an exact Task type"));
            };
            match result {
                Some(result)
                    if slot(*result)?.ty == **output
                        && normal.is_some()
                        && **output != ResolvedTy::Never => {}
                None if **output == ResolvedTy::Unit && normal.is_some() => {}
                None if **output == ResolvedTy::Never && normal.is_none() => {}
                _ => return Err(PhysicalError::new("await output differs from task result")),
            }
            if let Some(normal) = normal {
                edge(normal)?;
            }
            edge(cancel)?;
            edge(unwind)
        }
        PhysicalTerminator::TaskScopeJoin { normal, unwind, .. } => {
            edge(normal)?;
            edge(unwind)
        }
        PhysicalTerminator::NativeIo {
            operation,
            args,
            result,
            normal,
            cancel,
            unwind,
        } => {
            let arguments = args
                .iter()
                .map(|argument| match argument {
                    ArgumentTransfer::Borrow(id) => Ok(slot(*id)?.ty.clone()),
                    _ => Err(PhysicalError::new("native I/O requires borrowed inputs")),
                })
                .collect::<Result<Vec<_>, PhysicalError>>()?;
            if !operation
                .contract()
                .matches_signature(&arguments, &slot(*result)?.ty)
            {
                return Err(PhysicalError::new(
                    "native I/O inputs or result differ from the operation contract",
                ));
            }
            edge(normal)?;
            edge(cancel)?;
            edge(unwind)
        }
        PhysicalTerminator::Sleep {
            duration,
            normal,
            cancel,
            unwind,
        } => {
            if slot(*duration)?.ty != ResolvedTy::Duration || slot(*duration)?.own != OwnKind::None
            {
                return Err(PhysicalError::new("sleep input must be a trivial duration"));
            }
            edge(normal)?;
            edge(cancel)?;
            edge(unwind)
        }
        PhysicalTerminator::SleepUntil {
            deadline,
            normal,
            cancel,
            unwind,
        } => {
            if slot(*deadline)?.ty != ResolvedTy::I64 || slot(*deadline)?.own != OwnKind::None {
                return Err(PhysicalError::new(
                    "sleep-until input must be a trivial instant",
                ));
            }
            edge(normal)?;
            edge(cancel)?;
            edge(unwind)
        }
        PhysicalTerminator::EnterDefer { body, .. }
        | PhysicalTerminator::FinishDefer { next: body, .. }
        | PhysicalTerminator::CheckedRaiseFault { cleanup: body, .. } => edge(body),
        PhysicalTerminator::RecoverFault {
            result,
            glue,
            deadline_variant,
            fault_variant,
            normal,
            unwind,
        } => {
            let glue = variant_glue(module, *glue)?;
            if slot(*result)?.ty != glue.ty
                || slot(*result)?.own != OwnKind::Owned
                || glue.is_indirect
                || deadline_variant == fault_variant
                || glue.variants.len() != 2
                || [*deadline_variant, *fault_variant].iter().any(|tag| {
                    glue.variants.get(*tag as usize).is_none_or(|variant| {
                        variant.fields.len() != 1 || variant.fields[0].ty != ResolvedTy::String
                    })
                })
            {
                return Err(PhysicalError::new(
                    "scope recovery requires owned string failure variants",
                ));
            }
            edge(normal)?;
            edge(unwind)
        }
        PhysicalTerminator::CleanupDispatch { normal, fault } => {
            edge(normal)?;
            edge(fault)
        }
        PhysicalTerminator::DynCall {
            receiver,
            slot,
            signature,
            args,
            result,
            normal,
            unwind,
        } => {
            callable::verify_dyn_call(
                module, function, *receiver, *slot, signature, args, *result,
            )?;
            if normal.is_none() != (signature.return_ty == ResolvedTy::Never) {
                return Err(PhysicalError::new(
                    "call normal edge differs from its return type",
                ));
            }
            if let Some(normal) = normal {
                edge(normal)?;
            }
            edge(unwind.as_ref().ok_or_else(|| {
                PhysicalError::new("dynamic dispatch requires a fault cleanup edge")
            })?)?;
            Ok(())
        }
        PhysicalTerminator::IndirectCall {
            callee,
            signature,
            args,
            result,
            normal,
            unwind,
        } => {
            callable::verify_indirect_call(module, function, *callee, signature, args, *result)?;
            if normal.is_none() != (signature.return_ty == ResolvedTy::Never) {
                return Err(PhysicalError::new(
                    "call normal edge differs from its return type",
                ));
            }
            if let Some(normal) = normal {
                edge(normal)?;
            }
            edge(unwind.as_ref().ok_or_else(|| {
                PhysicalError::new("indirect invocation requires a fault cleanup edge")
            })?)?;
            Ok(())
        }

        PhysicalTerminator::Return { value } => {
            let returned = value.map(|value| match value {
                ReturnTransfer::Borrow(id)
                | ReturnTransfer::Move(id)
                | ReturnTransfer::Clone { source: id, .. } => id,
            });
            if let Some(id) = returned {
                if slot(id)?.own == OwnKind::Guaranteed {
                    return Err(PhysicalError::new(
                        "physical function return cannot escape guaranteed storage",
                    ));
                }
            }
            match (
                &callable_for(module, function.callable)?.return_ty,
                returned,
            ) {
                (ResolvedTy::Unit, None) => Ok(()),
                (ResolvedTy::Unit, Some(_)) | (_, None) => Err(PhysicalError::new(
                    "physical return/result-out presence disagrees with callable ABI",
                )),
                (expected, Some(id)) if &slot(id)?.ty == expected => Ok(()),
                _ => Err(PhysicalError::new(
                    "physical return storage type disagrees with callable ABI",
                )),
            }
        }
        PhysicalTerminator::Goto(target) => edge(target),
        PhysicalTerminator::Branch {
            condition,
            then_target,
            else_target,
        } => {
            if slot(*condition)?.ty != ResolvedTy::Bool {
                return Err(PhysicalError::new(
                    "physical branch condition is not bool storage",
                ));
            }
            edge(then_target)?;
            edge(else_target)
        }
        PhysicalTerminator::SwitchVariant {
            scrutinee,
            glue,
            arms,
        } => {
            let scrutinee = slot(*scrutinee)?;
            let recipe = variant_glue(module, *glue)?;
            // A loaned scrutinee reads the same descriptor without taking the
            // recipe's release obligation.
            let own_agrees = scrutinee.own == recipe.own || scrutinee.own == OwnKind::Guaranteed;
            if scrutinee.ty != recipe.ty || !own_agrees {
                return Err(PhysicalError::new(
                    "physical variant switch source disagrees with its glue recipe",
                ));
            }
            if arms.len() != recipe.variants.len() {
                return Err(PhysicalError::new(
                    "physical variant switch is not exhaustive",
                ));
            }
            let mut seen = BTreeSet::new();
            for arm in arms {
                if !seen.insert(arm.variant) {
                    return Err(PhysicalError::new(
                        "physical variant switch repeats a declaration-order tag",
                    ));
                }
                let case = usize::try_from(arm.variant)
                    .ok()
                    .and_then(|variant| recipe.variants.get(variant))
                    .ok_or_else(|| {
                        PhysicalError::new(format!(
                            "physical variant switch tag {} is out of bounds",
                            arm.variant
                        ))
                    })?;
                if arm.fields.len() != case.fields.len() {
                    return Err(PhysicalError::new(format!(
                        "physical variant switch tag {} has {} fields for {} recipes",
                        arm.variant,
                        arm.fields.len(),
                        case.fields.len()
                    )));
                }
                for (field, expected) in arm.fields.iter().zip(&case.fields) {
                    let field = slot(*field)?;
                    // A loaned scrutinee hands its payloads out as loans of the
                    // same region rather than as the recipe's owners.
                    let own_agrees = field.own == expected.own
                        || (scrutinee.own == OwnKind::Guaranteed
                            && field.own == OwnKind::Guaranteed);
                    if field.ty != expected.ty || !own_agrees {
                        return Err(PhysicalError::new(format!(
                            "physical variant switch tag {} payload disagrees with its recipe",
                            arm.variant
                        )));
                    }
                }
                edge(&arm.target)?;
            }
            Ok(())
        }
        PhysicalTerminator::CheckedBinary {
            op,
            lhs,
            rhs,
            result,
            normal,
            failures,
        } => {
            if !hew_sir::checked_binary_types_match(
                *op,
                &slot(*lhs)?.ty,
                &slot(*rhs)?.ty,
                &slot(*result)?.ty,
            ) {
                return Err(PhysicalError::new(
                    "physical checked binary type relation is invalid",
                ));
            }
            let ty = &slot(*result)?.ty;
            let required = hew_sir::checked_binary_failure_kinds(*op, ty).ok_or_else(|| {
                PhysicalError::new(
                    "physical checked binary uses an operator or type without checked failures",
                )
            })?;
            if failures
                .iter()
                .map(|failure| failure.kind)
                .ne(required.iter().copied())
            {
                return Err(PhysicalError::new(
                    "physical checked binary failure set disagrees with SIR semantics",
                ));
            }
            edge(normal)?;
            for failure in failures {
                edge(&failure.edge)?;
            }
            Ok(())
        }
        PhysicalTerminator::Call {
            callee,
            args,
            result,
            normal,
            unwind,
            handback,
        } => {
            let callee = callable_for(module, *callee)?;
            if handback.is_some() != callee.receiver_handback
                || handback.is_some_and(|handback| {
                    result.is_none()
                        || callee.params.first().is_none_or(|receiver| {
                            slot(handback).is_ok_and(|slot| slot.ty != receiver.ty)
                        })
                })
            {
                return Err(PhysicalError::new(
                    "physical call receiver handback disagrees with its `var self` callee",
                ));
            }
            if args.len() != callee.params.len() {
                return Err(PhysicalError::new(format!(
                    "physical call to {} has {} arguments for {} parameters",
                    callee.id.0,
                    args.len(),
                    callee.params.len()
                )));
            }
            for (argument, parameter) in args.iter().zip(&callee.params) {
                let id = match argument {
                    ArgumentTransfer::Borrow(id)
                    | ArgumentTransfer::BorrowMut(id)
                    | ArgumentTransfer::Move(id)
                    | ArgumentTransfer::Clone { source: id, .. } => *id,
                };
                if slot(id)?.ty != parameter.ty {
                    return Err(PhysicalError::new(
                        "physical call argument type disagrees with callee ABI",
                    ));
                }
                let valid_transfer = match parameter.passing {
                    SemParamPassing::ReadOnly => {
                        !matches!(argument, ArgumentTransfer::BorrowMut(_))
                    }
                    SemParamPassing::Borrow => matches!(argument, ArgumentTransfer::Borrow(_)),
                    SemParamPassing::BorrowMut => {
                        matches!(argument, ArgumentTransfer::BorrowMut(_))
                    }
                    SemParamPassing::Consume => {
                        matches!(argument, ArgumentTransfer::Move(_))
                            && slot(id)?.own == OwnKind::Owned
                    }
                };
                if !valid_transfer {
                    return Err(PhysicalError::new(
                        "physical call argument transfer disagrees with parameter passing",
                    ));
                }
            }
            match (&callee.return_ty, result) {
                (ResolvedTy::Unit | ResolvedTy::Never, None) => {}
                (ResolvedTy::Unit | ResolvedTy::Never, Some(_)) | (_, None) => {
                    return Err(PhysicalError::new(
                        "physical call result-out presence disagrees with callee ABI",
                    ));
                }
                (expected, Some(id))
                    if hew_sir::call_boundary_types_match(&slot(*id)?.ty, expected) => {}
                _ => {
                    return Err(PhysicalError::new(
                        "physical call result storage type disagrees with callee ABI",
                    ));
                }
            }
            if normal.is_none() != (callee.return_ty == ResolvedTy::Never) {
                return Err(PhysicalError::new(
                    "call normal edge differs from its return type",
                ));
            }
            if let Some(normal) = normal {
                edge(normal)?;
            }
            if let Some(unwind) = unwind {
                edge(unwind)?;
            }
            Ok(())
        }
        PhysicalTerminator::WireCodec {
            direction,
            plan,
            recipes,
            text_result,
            input,
            result,
            normal,
            unwind,
        } => {
            wire::verify_wire_plan(module, plan)?;
            let ArgumentTransfer::Borrow(source) = input else {
                return Err(PhysicalError::new("wire codec input must borrow its slot"));
            };
            let input_ty = if direction.is_serialize() {
                plan.ty.clone()
            } else if direction.is_text() {
                ResolvedTy::String
            } else {
                ResolvedTy::Bytes
            };
            if slot(*source)?.ty != input_ty {
                return Err(PhysicalError::new(
                    "wire codec input type differs from its schema",
                ));
            }
            let output = &slot(*result)?.ty;
            let valid_result = match direction {
                hew_types::WireCodecDirection::Encode => *output == ResolvedTy::Bytes,
                hew_types::WireCodecDirection::Decode => *output == plan.ty,
                hew_types::WireCodecDirection::ToJson | hew_types::WireCodecDirection::ToYaml => {
                    *output == ResolvedTy::String
                }
                hew_types::WireCodecDirection::FromJson
                | hew_types::WireCodecDirection::FromYaml => {
                    matches!(output, ResolvedTy::Named { builtin: Some(hew_types::BuiltinType::Result), args, .. } if args == &[plan.ty.clone(), ResolvedTy::String])
                }
            };
            if !valid_result {
                return Err(PhysicalError::new(
                    "wire codec result type differs from its direction",
                ));
            }
            if let Some(cases) = text_result {
                let glue = module
                    .variant_glue
                    .get(cases.glue.0 as usize)
                    .filter(|glue| glue.ty == *output)
                    .ok_or_else(|| {
                        PhysicalError::new("wire text Result has no exact physical glue")
                    })?;
                if cases.ok == cases.error
                    || glue.variants.len() != 2
                    || !glue
                        .variants
                        .get(cases.ok as usize)
                        .is_some_and(|case| case.fields.len() == 1 && case.fields[0].ty == plan.ty)
                    || !glue.variants.get(cases.error as usize).is_some_and(|case| {
                        case.fields.len() == 1 && case.fields[0].ty == ResolvedTy::String
                    })
                {
                    return Err(PhysicalError::new(
                        "wire text Result cases disagree with payloads",
                    ));
                }
            }
            if text_result.is_some()
                != matches!(
                    direction,
                    hew_types::WireCodecDirection::FromJson
                        | hew_types::WireCodecDirection::FromYaml
                )
            {
                return Err(PhysicalError::new(
                    "wire Result cases differ from codec direction",
                ));
            }
            let mut expected = BTreeSet::new();
            plan.visit_types(&mut |ty| {
                expected.insert(ty.clone());
            });
            if recipes.keys().cloned().collect::<BTreeSet<_>>() != expected {
                return Err(PhysicalError::new(
                    "wire codec value recipes do not cover its exact schema",
                ));
            }
            for (ty, recipe) in recipes {
                if *ty != recipe.ty {
                    return Err(PhysicalError::new(
                        "wire recipe key differs from its value type",
                    ));
                }
                verify_value_recipe(module, recipe)?;
            }
            edge(normal)?;
            edge(unwind)
        }
        PhysicalTerminator::ValueCall {
            ty,
            capability,
            args,
            result,
            normal,
            unwind,
        } => {
            if !module
                .value_capabilities
                .contains_key(&(ty.clone(), *capability))
            {
                return Err(PhysicalError::new(
                    "physical value call lacks its exact selected capability",
                ));
            }
            let (arity, result_ty) = match capability {
                ValueCapability::Hash => (1, ResolvedTy::I64),
                ValueCapability::Eq => (2, ResolvedTy::Bool),
            };
            if args.len() != arity {
                return Err(PhysicalError::new(
                    "physical value call has the wrong callback arity",
                ));
            }
            for argument in args {
                let ArgumentTransfer::Borrow(source) = argument else {
                    return Err(PhysicalError::new(
                        "physical value call arguments must borrow their slots",
                    ));
                };
                if &slot(*source)?.ty != ty {
                    return Err(PhysicalError::new(
                        "physical value call argument has another type",
                    ));
                }
            }
            let result = slot(*result)?;
            if result.ty != result_ty || result.own != OwnKind::None {
                return Err(PhysicalError::new(
                    "physical value call requires its scalar callback result",
                ));
            }
            edge(normal)?;
            edge(unwind)
        }
        PhysicalTerminator::ActorCall {
            operation,
            args,
            result,
            normal,
            unwind,
        } => {
            let signature = actor_signature(module, operation)?;
            if args.len() != signature.params.len() {
                return Err(PhysicalError::new(
                    "actor boundary argument count differs from protocol",
                ));
            }
            for (argument, parameter) in args.iter().zip(&signature.params) {
                let ((SemParamPassing::Borrow, ArgumentTransfer::Borrow(id))
                | (SemParamPassing::Consume, ArgumentTransfer::Move(id))) =
                    (parameter.passing, argument)
                else {
                    return Err(PhysicalError::new(
                        "actor boundary changes its argument ownership",
                    ));
                };
                if slot(*id)?.ty != parameter.ty {
                    return Err(PhysicalError::new(
                        "actor payload differs from its protocol type",
                    ));
                }
            }
            match result {
                None if signature.return_ty == ResolvedTy::Unit => {}
                Some(id) if slot(*id)?.ty == signature.return_ty => {}
                _ => {
                    return Err(PhysicalError::new(
                        "actor boundary result differs from its protocol",
                    ))
                }
            }
            let unwind = unwind
                .as_ref()
                .ok_or_else(|| PhysicalError::new("actor boundary lacks fault cleanup"))?;
            edge(normal)?;
            edge(unwind)
        }
        PhysicalTerminator::ExternCall {
            symbol,
            args,
            result,
            result_abi,
            normal,
            ..
        } => {
            // SIR proved types and ownership against the declaration. Verify
            // the transfer/edge structure and the target's C result carrier.
            for argument in args {
                let id = match argument {
                    ArgumentTransfer::Borrow(id)
                    | ArgumentTransfer::BorrowMut(id)
                    | ArgumentTransfer::Move(id)
                    | ArgumentTransfer::Clone { source: id, .. } => *id,
                };
                slot(id)?;
            }
            if symbol.is_empty() {
                return Err(PhysicalError::new("extern call has no linker symbol"));
            }
            let result_ty = match result {
                Some(result) => &slot(*result)?.ty,
                None => &ResolvedTy::Unit,
            };
            if *result_abi != module.target.extern_result_abi(result_ty)? {
                return Err(PhysicalError::new(
                    "extern result ABI differs from its target classification",
                ));
            }
            edge(normal)
        }
        PhysicalTerminator::RuntimeCall {
            action,
            args,
            result,
            normal,
            failure,
        } => {
            let contract = action
                .family
                .semantic_contract()
                .ok_or_else(|| PhysicalError::new("physical runtime action lost its contract"))?;
            if args.len() != contract.arguments.len() {
                return Err(PhysicalError::new(format!(
                    "physical runtime action {action:?} has {} arguments for {} parameters",
                    args.len(),
                    contract.arguments.len()
                )));
            }
            let parameter_types = args
                .iter()
                .map(|argument| {
                    let id = match argument {
                        ArgumentTransfer::Borrow(id)
                        | ArgumentTransfer::BorrowMut(id)
                        | ArgumentTransfer::Move(id)
                        | ArgumentTransfer::Clone { source: id, .. } => *id,
                    };
                    Ok(slot(id)?.ty.clone())
                })
                .collect::<Result<Vec<_>, PhysicalError>>()?;
            let result_type = result
                .map(|id| slot(id).map(|slot| &slot.ty))
                .transpose()?
                .unwrap_or(if matches!(contract.result, RuntimeResultEffect::Never) {
                    &ResolvedTy::Never
                } else {
                    &ResolvedTy::Unit
                });
            let signature = contract.instantiate(&parameter_types, result_type)
                .map_err(|reason| PhysicalError::new(format!("physical runtime action {action:?} signature disagrees with its semantic contract: {reason}")))?;
            if &signature.result_ty != result_type {
                return Err(PhysicalError::new(format!("physical runtime action {action:?} result type disagrees with its semantic contract")));
            }
            for (argument, expected) in args.iter().zip(contract.arguments) {
                let (id, actual_effect) = match argument {
                    ArgumentTransfer::Borrow(id) => (*id, RuntimeArgumentEffect::Borrow),
                    ArgumentTransfer::BorrowMut(_) => {
                        return Err(PhysicalError::new(
                            "physical runtime operation has no exclusive argument contract",
                        ));
                    }
                    ArgumentTransfer::Move(id) => {
                        if slot(*id)?.own != OwnKind::Owned {
                            return Err(PhysicalError::new(format!(
                                "physical runtime action {action:?} moves a non-owned argument"
                            )));
                        }
                        (*id, RuntimeArgumentEffect::Move)
                    }
                    ArgumentTransfer::Clone {
                        source,
                        action: clone_action,
                    } => {
                        if *clone_action != CloneAction::Bitwise
                            || slot(*source)?.own != OwnKind::None
                        {
                            return Err(PhysicalError::new(format!(
                                "physical runtime action {action:?} copies through a non-bitwise action"
                            )));
                        }
                        (*source, RuntimeArgumentEffect::Copy)
                    }
                };
                let _ = slot(id)?;
                if actual_effect
                    != expected
                        .effect
                        .resolve_operand(semantic_type_facts(module, &slot(id)?.ty)?.class)
                {
                    return Err(PhysicalError::new(format!(
                        "physical runtime action {action:?} argument disagrees with its semantic contract"
                    )));
                }
            }
            if let PhysicalRuntimeCarrier::PairWithOption {
                pair,
                option: descriptor,
            } = &action.carrier
            {
                let Some(output) = result else {
                    return Err(PhysicalError::new("bytes pop has no result storage"));
                };
                let pair = aggregate_glue(module, *pair)?;
                let descriptor = variant_glue(module, *descriptor)?;
                let [receiver, optional] = pair.fields.as_slice() else {
                    return Err(PhysicalError::new("bytes pop pair is not two fields"));
                };
                if pair.ty != slot(*output)?.ty
                    || receiver.ty != ResolvedTy::Bytes
                    || descriptor.ty != optional.ty
                    || descriptor.is_indirect
                    || descriptor.variants.len() != 2
                    || descriptor.variants[0].fields.len() != 1
                    || descriptor.variants[0].fields[0].ty != ResolvedTy::U8
                    || !descriptor.variants[1].fields.is_empty()
                {
                    return Err(PhysicalError::new(
                        "bytes pop descriptor disagrees with result",
                    ));
                }
            }
            // The scalar optional reads publish an exact payload type; other
            // variant results carry their own checked type and are verified by
            // their operation's contract.
            let scalar_optional = match action.family {
                RuntimeCallFamily::StringCharAt => Some(ResolvedTy::Char),
                RuntimeCallFamily::StringFind | RuntimeCallFamily::StringCharAtUtf8 => {
                    Some(ResolvedTy::I64)
                }
                RuntimeCallFamily::BytesGet => Some(ResolvedTy::U8),
                _ => None,
            };
            if let (PhysicalRuntimeCarrier::Variant(descriptor), Some(payload)) =
                (&action.carrier, scalar_optional)
            {
                let Some(output) = result else {
                    return Err(PhysicalError::new("string find has no result storage"));
                };
                let descriptor = variant_glue(module, *descriptor)?;
                if descriptor.ty != slot(*output)?.ty
                    || descriptor.is_indirect
                    || descriptor.variants.len() != 2
                    || descriptor.variants[0].fields.len() != 1
                    || descriptor.variants[0].fields[0].ty != payload
                    || !descriptor.variants[1].fields.is_empty()
                {
                    return Err(PhysicalError::new(
                        "optional runtime descriptor disagrees with result",
                    ));
                }
            }
            match (contract.result, result) {
                (RuntimeResultEffect::FreshOwnedVariant(kind), Some(id)) => {
                    let result_slot = slot(*id)?;
                    let PhysicalRuntimeCarrier::Utf8Decode {
                        result: result_glue,
                        error,
                        error_len,
                    } = action.carrier
                    else {
                        return Err(PhysicalError::new(
                            "variant runtime result has no physical contract",
                        ));
                    };
                    let Some((ok_ty, error_ty)) = kind.payload_types(&result_slot.ty) else {
                        return Err(PhysicalError::new(
                            "UTF-8 decode result has the wrong nominal type",
                        ));
                    };
                    let result_glue = variant_glue(module, result_glue)?;
                    let error_glue = aggregate_glue(module, error)?;
                    let option_glue = variant_glue(module, error_len)?;
                    let option_is_i64 = matches!(
                        &option_glue.ty,
                        ResolvedTy::Named { builtin: Some(hew_types::BuiltinType::Option), args, .. }
                            if args.as_slice() == [ResolvedTy::I64]
                    );
                    let field_is = |fields: &[PhysicalValueRecipe], ty: &ResolvedTy| {
                        fields.len() == 1 && &fields[0].ty == ty
                    };
                    if result_slot.own != OwnKind::Owned
                        || result_glue.ty != result_slot.ty
                        || result_glue.is_indirect
                        || result_glue.variants.len() != 2
                        || !field_is(&result_glue.variants[0].fields, ok_ty)
                        || !field_is(&result_glue.variants[1].fields, error_ty)
                        || &error_glue.ty != error_ty
                        || error_glue.fields.len() != 2
                        || error_glue.fields[0].ty != ResolvedTy::I64
                        || error_glue.fields[1].ty != option_glue.ty
                        || !option_is_i64
                        || option_glue.is_indirect
                        || option_glue.variants.len() != 2
                        || !field_is(&option_glue.variants[0].fields, &ResolvedTy::I64)
                        || !option_glue.variants[1].fields.is_empty()
                    {
                        return Err(PhysicalError::new(
                            "UTF-8 decode physical descriptors disagree with its result contract",
                        ));
                    }
                }
                (RuntimeResultEffect::Unit | RuntimeResultEffect::Never, None) => {}
                (RuntimeResultEffect::Unit | RuntimeResultEffect::Never, Some(_)) | (_, None) => {
                    return Err(PhysicalError::new(format!(
                        "physical runtime action {action:?} result presence disagrees with its semantic contract"
                    )));
                }
                (
                    RuntimeResultEffect::BitCopy(_)
                    | RuntimeResultEffect::Borrowed(_)
                    | RuntimeResultEffect::FreshOwned(_)
                    | RuntimeResultEffect::UpdatedReceiver(_)
                    | RuntimeResultEffect::IndependentValue(_)
                    | RuntimeResultEffect::UpdatedReceiverAndValue(_),
                    Some(id),
                ) => {
                    let expected_own = match contract.result {
                        RuntimeResultEffect::BitCopy(_) => OwnKind::None,
                        // A loan of a result that carries no obligation is the
                        // value itself; SIR's loan rule decided which it is.
                        RuntimeResultEffect::Borrowed(_) => {
                            match OwnKind::of_class(semantic_type_facts(module, result_type)?.class)
                            {
                                OwnKind::None => OwnKind::None,
                                _ => OwnKind::Guaranteed,
                            }
                        }
                        RuntimeResultEffect::FreshOwned(_)
                        | RuntimeResultEffect::UpdatedReceiver(_)
                        | RuntimeResultEffect::UpdatedReceiverAndValue(_) => OwnKind::Owned,
                        RuntimeResultEffect::IndependentValue(_) => {
                            OwnKind::of_class(semantic_type_facts(module, result_type)?.class)
                        }
                        RuntimeResultEffect::Unit
                        | RuntimeResultEffect::Never
                        | RuntimeResultEffect::FreshOwnedVariant(_) => {
                            unreachable!()
                        }
                    };
                    if slot(*id)?.own != expected_own {
                        return Err(PhysicalError::new(format!(
                            "physical runtime action {action:?} result ownership disagrees with its semantic contract"
                        )));
                    }
                }
            }
            match action.carrier {
                PhysicalRuntimeCarrier::Vector { operation, glue } => {
                    if matches!(action.family, RuntimeCallFamily::Array(_))
                        && !matches!(vector_glue(module, glue)?.ty, ResolvedTy::Array(_, _))
                    {
                        return Err(PhysicalError::new(
                            "array operation has a non-array descriptor",
                        ));
                    }
                    verify_vector_call(module, operation, glue, &parameter_types, result_type)?;
                }
                PhysicalRuntimeCarrier::Map { operation, glue } => {
                    verify_map_call(module, operation, glue, &parameter_types, result_type)?;
                }
                PhysicalRuntimeCarrier::Set { operation, glue } => {
                    verify_set_call(module, operation, glue, &parameter_types, result_type)?;
                }
                _ => {}
            }
            match (contract.failures.is_empty(), failure) {
                (true, None) | (false, Some(_)) => {}
                _ => {
                    return Err(PhysicalError::new(format!(
                        "physical runtime action {action:?} failure edge disagrees with its semantic contract"
                    )));
                }
            }
            edge(normal)?;
            if let Some(failure) = failure {
                edge(failure)?;
            }
            Ok(())
        }
        PhysicalTerminator::Panic { message, cleanup } => {
            let ArgumentTransfer::Borrow(source) = message else {
                return Err(PhysicalError::new("physical panic must borrow its message"));
            };
            if slot(*source)?.ty != ResolvedTy::String {
                return Err(PhysicalError::new("physical panic message must be String"));
            }
            edge(cleanup)
        }
        PhysicalTerminator::PropagateFault { handback } => {
            let own = callable_for(module, function.callable)?;
            if handback.is_some() != own.receiver_handback
                || handback.is_some_and(|handback| {
                    own.params.first().is_none_or(|receiver| {
                        slot(handback).is_ok_and(|slot| slot.ty != receiver.ty)
                    })
                })
            {
                return Err(PhysicalError::new(
                    "physical fault exit hands back exactly its `var self` receiver",
                ));
            }
            Ok(())
        }
        PhysicalTerminator::Trap(_) | PhysicalTerminator::Unreachable => Ok(()),
    }
}
