//! Realize the checked generator contracts without inventing ownership or frames.

use super::{
    apply_edge, call_successors, defer, initialized, physical_value_recipe,
    require_no_live_borrows, storage, verify_value_recipe, ArgumentTransfer, BlockId, BuiltinType,
    CallResult, ClosureId, FaultState, FlowState, FunctionLowerer, InitState, OwnKind,
    PhysicalError, PhysicalFunction, PhysicalModule, PhysicalOp, PhysicalTerminator, ResolvedTy,
    SemOp, SemTerminator, StorageOrigin,
};

impl FunctionLowerer<'_> {
    pub(super) fn lower_generator_make(
        &self,
        operation: &SemOp,
        closure: ClosureId,
        callable: &hew_sir::Operand,
    ) -> Result<PhysicalOp, PhysicalError> {
        let dest = self.one_result(operation)?;
        let (yielded, returned) = hew_sir::generator_parts(&self.storage[dest.0 as usize].ty)
            .ok_or_else(|| PhysicalError::new("generator construction has no output contract"))?;
        Ok(PhysicalOp::GeneratorMake {
            closure,
            callable: self.value(callable.value)?,
            dest,
            yielded: physical_value_recipe(self.module, self.glue_ids, yielded)?,
            returned: physical_value_recipe(self.module, self.glue_ids, returned)?,
        })
    }

    pub(super) fn lower_generator_suspend(
        &self,
        terminator: &SemTerminator,
    ) -> Result<PhysicalTerminator, PhysicalError> {
        let SemTerminator::Suspend {
            kind,
            inputs,
            result,
            resumes,
            cancel,
            unwind,
        } = terminator
        else {
            unreachable!()
        };
        let normal = self.lower_edge(&resumes[0])?;
        Ok(match kind {
            hew_sir::SuspendKind::Yield => PhysicalTerminator::GeneratorYield {
                value: self.argument_transfers(inputs)?[0],
                normal,
                cancel: self.lower_edge(cancel)?,
                unwind: self.lower_edge(unwind)?,
            },
            hew_sir::SuspendKind::GeneratorNext => {
                let CallResult::Value(result) = result else {
                    return Err(PhysicalError::new(
                        "generator next requires its Option result",
                    ));
                };
                PhysicalTerminator::GeneratorNext {
                    generator: self.argument_transfers(inputs)?[0],
                    result: self.value(result.id)?,
                    normal,
                    cancel: self.lower_edge(cancel)?,
                    unwind: self.lower_edge(unwind)?,
                }
            }
            hew_sir::SuspendKind::GeneratorClose { place } => PhysicalTerminator::GeneratorClose {
                generator: if let Some(place) = place {
                    self.place(*place)?
                } else {
                    self.value(inputs[0].operand.value)?
                },
                conditional: place.is_some(),
                next: normal,
            },
            _ => unreachable!(),
        })
    }
}

pub(super) fn verify_make(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    operation: &PhysicalOp,
) -> Result<(), PhysicalError> {
    let PhysicalOp::GeneratorMake {
        closure,
        callable,
        dest,
        yielded,
        returned,
    } = operation
    else {
        unreachable!()
    };
    let producer = module
        .closures
        .get(closure.0 as usize)
        .filter(|producer| producer.id == *closure)
        .ok_or_else(|| PhysicalError::new("generator has no producer descriptor"))?;
    let (params, output, capabilities) =
        hew_sir::callable_parts(&producer.ty).map_err(PhysicalError::new)?;
    if storage(function, *callable)?.ty != producer.ty
        || producer.generator_yield.as_ref() != Some(&yielded.ty)
        || output != &returned.ty
        || !params.is_empty()
        || capabilities.call != hew_types::CallableCallMode::Once
        || hew_sir::generator_parts(&storage(function, *dest)?.ty)
            != Some((&yielded.ty, &returned.ty))
    {
        return Err(PhysicalError::new(
            "generator construction changes its exact producer contract",
        ));
    }
    verify_value_recipe(module, yielded)?;
    verify_value_recipe(module, returned)
}

pub(super) fn verify_suspend(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    terminator: &PhysicalTerminator,
) -> Result<(), PhysicalError> {
    match terminator {
        PhysicalTerminator::GeneratorYield { value, .. } => {
            let ArgumentTransfer::Move(value) = value else {
                return Err(PhysicalError::new(
                    "generator yield must transfer its value",
                ));
            };
            let producer = module
                .closures
                .iter()
                .find(|producer| producer.body == function.callable)
                .ok_or_else(|| PhysicalError::new("yield has no producer body"))?;
            if producer.generator_yield.as_ref() != Some(&storage(function, *value)?.ty) {
                return Err(PhysicalError::new("yield changes its producer output type"));
            }
        }
        PhysicalTerminator::GeneratorNext {
            generator, result, ..
        } => {
            let ArgumentTransfer::BorrowMut(generator) = generator else {
                return Err(PhysicalError::new(
                    "generator next requires an exclusive receiver",
                ));
            };
            let (yielded, _) = hew_sir::generator_parts(&storage(function, *generator)?.ty)
                .ok_or_else(|| PhysicalError::new("next has no generator receiver"))?;
            if storage(function, *result)?.ty
                != ResolvedTy::named_builtin("Option", BuiltinType::Option, vec![yielded.clone()])
            {
                return Err(PhysicalError::new("generator next changes its output type"));
            }
        }
        PhysicalTerminator::GeneratorClose {
            generator,
            conditional,
            ..
        } => {
            let slot = storage(function, *generator)?;
            if hew_sir::generator_parts(&slot.ty).is_none()
                || slot.own != OwnKind::Owned
                || (*conditional && !matches!(slot.origin, StorageOrigin::Local(_)))
            {
                return Err(PhysicalError::new(
                    "generator close lacks its initialized owner contract",
                ));
            }
        }
        _ => unreachable!(),
    }
    Ok(())
}

pub(super) fn successors(
    function: &PhysicalFunction,
    terminator: &PhysicalTerminator,
    mut state: FlowState,
    block: BlockId,
) -> Result<Vec<(BlockId, FlowState)>, PhysicalError> {
    let (input, result, normal, cancel, unwind) = match terminator {
        PhysicalTerminator::GeneratorYield {
            value,
            normal,
            cancel,
            unwind,
        } => (*value, None, normal, cancel, unwind),
        PhysicalTerminator::GeneratorNext {
            generator,
            result,
            normal,
            cancel,
            unwind,
        } => (*generator, Some(*result), normal, cancel, unwind),
        PhysicalTerminator::GeneratorClose {
            generator,
            conditional,
            next,
        } => {
            if *conditional {
                if state.active[generator.0 as usize] != InitState::Initialized {
                    return Err(PhysicalError::new(
                        "generator close requires active local storage",
                    ));
                }
            } else {
                initialized(function, &state, *generator, block, "generator close")?;
            }
            require_no_live_borrows(function, &state, *generator)?;
            if state.fault != FaultState::Active {
                state.fault = FaultState::MaybeActive;
            }
            state.exit |= defer::TRAP;
            return Ok(vec![apply_edge(function, next, state, block)?]);
        }
        _ => unreachable!(),
    };
    let mut successors = call_successors(
        function,
        &[input],
        result,
        normal,
        Some(unwind),
        state.clone(),
        block,
    )?;
    let (_, mut cancelled) = call_successors(
        function,
        &[input],
        result,
        normal,
        Some(cancel),
        state,
        block,
    )?
    .pop()
    .expect("cancellation successor");
    cancelled.exit = defer::CANCEL;
    successors.push((cancel.target, cancelled));
    Ok(successors)
}
