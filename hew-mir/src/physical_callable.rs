//! Physical construction, receiver and capture-storage contracts.

use super::{
    param_carrier, required_layout, semantic_type_facts, storage, ArgumentTransfer, BTreeSet,
    CallResult, CallUnwind, CallableId, ClosureId, FlowState, FunctionLowerer, InitState, OwnKind,
    ParamCarrier, PhysicalCallSignature, PhysicalClosure, PhysicalError, PhysicalFunction,
    PhysicalModule, PhysicalOp, PhysicalParam, PhysicalRepr, PhysicalTerminator, ResolvedTy,
    SemParamPassing, SemTerminator, StorageId, StorageOrigin,
};
use hew_types::CallableCallMode;

fn closure(module: &PhysicalModule, id: ClosureId) -> Result<&PhysicalClosure, PhysicalError> {
    module
        .closures
        .get(id.0 as usize)
        .filter(|closure| closure.id == id)
        .ok_or_else(|| PhysicalError::new("unknown physical closure identity"))
}

fn expected_receiver(ty: &ResolvedTy) -> Result<SemParamPassing, PhysicalError> {
    let (_, _, capabilities) = hew_sir::callable_parts(ty).map_err(PhysicalError::new)?;
    Ok(match capabilities.call {
        CallableCallMode::Read => SemParamPassing::Borrow,
        CallableCallMode::Var => SemParamPassing::BorrowMut,
        CallableCallMode::Once => SemParamPassing::Consume,
    })
}

pub(super) fn verify_closures(module: &PhysicalModule) -> Result<(), PhysicalError> {
    for (index, closure) in module.closures.iter().enumerate() {
        if usize::try_from(closure.id.0).ok() != Some(index) {
            return Err(PhysicalError::new(
                "physical closure has noncanonical identity",
            ));
        }
        let body = module
            .callables
            .get(closure.body.0 as usize)
            .filter(|body| body.id == closure.body)
            .ok_or_else(|| PhysicalError::new("physical closure lacks its exact body"))?;
        let (params, ret, _) = hew_sir::callable_parts(&closure.ty).map_err(PhysicalError::new)?;
        if body.instance != hew_sir::CallableInstance::Closure(closure.id)
            || body.params.len() != params.len() + 1
            || &body.return_ty != ret
            || body.params[0].ty != closure.ty
            || body.params[0].passing != expected_receiver(&closure.ty)?
            || body.params[0].carrier != ParamCarrier::Indirect
            || body.params[1..]
                .iter()
                .map(|param| &param.ty)
                .ne(params.iter())
            || !module
                .environment_glue
                .iter()
                .any(|glue| glue.ty == closure.ty)
        {
            return Err(PhysicalError::new(
                "physical closure body or environment disagrees with its contract",
            ));
        }
    }
    Ok(())
}

pub(super) fn verify_capture_slots(
    module: &PhysicalModule,
    function: &PhysicalFunction,
) -> Result<(), PhysicalError> {
    let captures = function
        .storage
        .iter()
        .filter_map(|slot| match slot.origin {
            StorageOrigin::Capture { environment, field } => Some((slot, environment, field)),
            _ => None,
        })
        .collect::<Vec<_>>();
    let body = &module.callables[function.callable.0 as usize];
    let hew_sir::CallableInstance::Closure(id) = body.instance else {
        return if captures.is_empty() {
            Ok(())
        } else {
            Err(PhysicalError::new(
                "capture storage belongs to a non-closure body",
            ))
        };
    };
    let closure = closure(module, id)?;
    let ResolvedTy::Closure {
        captures: fields, ..
    } = &closure.ty
    else {
        return Err(PhysicalError::new(
            "physical closure lacks concrete capture types",
        ));
    };
    if captures.len() != fields.len() {
        return Err(PhysicalError::new(
            "physical closure must address every canonical capture",
        ));
    }
    let mut seen = BTreeSet::new();
    for (slot, environment, field) in captures {
        if function.parameters.first() != Some(&environment)
            || fields.get(field as usize) != Some(&slot.ty)
            || slot.own != OwnKind::of_class(semantic_type_facts(module, &slot.ty)?.class)
            || !seen.insert(field)
        {
            return Err(PhysicalError::new(
                "physical capture differs from its canonical receiver field",
            ));
        }
    }
    Ok(())
}

pub(super) fn verify_function_make(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    dest: StorageId,
    callee: CallableId,
) -> Result<(), PhysicalError> {
    let dest = storage(function, dest)?;
    let callee = module
        .callables
        .get(callee.0 as usize)
        .filter(|row| row.id == callee)
        .ok_or_else(|| PhysicalError::new("function value lacks its exact callable"))?;
    let ResolvedTy::Function {
        params,
        ret,
        capabilities,
    } = &dest.ty
    else {
        return Err(PhysicalError::new(
            "function value requires a function carrier type",
        ));
    };
    if dest.own != OwnKind::Owned
        || capabilities.call != CallableCallMode::Read
        || !capabilities.clone
        || callee
            .params
            .iter()
            .map(|param| &param.ty)
            .ne(params.iter())
        || callee.return_ty != **ret
        || matches!(callee.instance, hew_sir::CallableInstance::Closure(_))
    {
        return Err(PhysicalError::new(
            "function value differs from its exact source signature",
        ));
    }
    Ok(())
}

pub(super) fn verify_closure_make(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    dest: StorageId,
    id: ClosureId,
    fields: &[StorageId],
) -> Result<(), PhysicalError> {
    let closure = closure(module, id)?;
    let dest = storage(function, dest)?;
    let ResolvedTy::Closure { captures, .. } = &closure.ty else {
        return Err(PhysicalError::new(
            "closure construction has no capture type",
        ));
    };
    if dest.ty != closure.ty || dest.own != OwnKind::Owned || fields.len() != captures.len() {
        return Err(PhysicalError::new(
            "closure construction differs from its exact environment",
        ));
    }
    let mut owned = BTreeSet::new();
    for (field, ty) in fields.iter().zip(captures) {
        let field = storage(function, *field)?;
        if &field.ty != ty
            || field.own != OwnKind::of_class(semantic_type_facts(module, ty)?.class)
            || (field.own == OwnKind::Owned && !owned.insert(field.id))
        {
            return Err(PhysicalError::new(
                "closure construction must transfer each exact capture owner once",
            ));
        }
    }
    Ok(())
}

pub(super) fn verify_indirect_call(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    callee: ArgumentTransfer,
    signature: &PhysicalCallSignature,
    args: &[ArgumentTransfer],
    result: Option<StorageId>,
) -> Result<(), PhysicalError> {
    let source = match callee {
        ArgumentTransfer::Borrow(source)
        | ArgumentTransfer::BorrowMut(source)
        | ArgumentTransfer::Move(source) => source,
        ArgumentTransfer::Clone { .. } => {
            return Err(PhysicalError::new(
                "indirect receiver requires an explicit borrow or transfer",
            ))
        }
    };
    let receiver = storage(function, source)?;
    let expected = expected_receiver(&receiver.ty)?;
    if !matches!(
        (expected, callee),
        (SemParamPassing::Borrow, ArgumentTransfer::Borrow(_))
            | (SemParamPassing::BorrowMut, ArgumentTransfer::BorrowMut(_))
            | (SemParamPassing::Consume, ArgumentTransfer::Move(_))
    ) || receiver.own == OwnKind::None
        || (expected == SemParamPassing::Consume && receiver.own != OwnKind::Owned)
    {
        return Err(PhysicalError::new(
            "indirect receiver transfer differs from its invocation capability",
        ));
    }
    let semantic = hew_sir::callable_value_signature(&receiver.ty, &module.type_facts)
        .map_err(PhysicalError::new)?;
    if signature.return_ty != semantic.return_ty
        || signature.params.len() != semantic.params.len()
        || args.len() != signature.params.len()
        || signature.return_layout.as_ref()
            != if matches!(semantic.return_ty, ResolvedTy::Unit | ResolvedTy::Never) {
                None
            } else {
                module.target.layout(&semantic.return_ty)
            }
    {
        return Err(PhysicalError::new(
            "indirect signature differs from its exact callable type",
        ));
    }
    for ((param, semantic), arg) in signature.params.iter().zip(&semantic.params).zip(args) {
        let source = match (param.passing, *arg) {
            (SemParamPassing::ReadOnly, ArgumentTransfer::Clone { source, action }) => {
                let slot = storage(function, source)?;
                super::verify_clone_action(module, &slot.ty, slot.own, action)?;
                source
            }
            (SemParamPassing::Borrow, ArgumentTransfer::Borrow(source))
            | (SemParamPassing::BorrowMut, ArgumentTransfer::BorrowMut(source))
            | (SemParamPassing::Consume, ArgumentTransfer::Move(source)) => source,
            _ => {
                return Err(PhysicalError::new(
                    "indirect user argument has an incompatible transfer",
                ));
            }
        };
        verify_argument(module, function, param, semantic, source)?;
    }
    match (result, &signature.return_ty) {
        (None, ResolvedTy::Unit | ResolvedTy::Never) => {}
        (Some(id), ty)
            if storage(function, id)?.ty == *ty
                && !matches!(ty, ResolvedTy::Unit | ResolvedTy::Never)
                && storage(function, id)?.own
                    == OwnKind::of_class(semantic_type_facts(module, ty)?.class) => {}
        _ => {
            return Err(PhysicalError::new(
                "indirect result-out differs from its signature",
            ))
        }
    }
    Ok(())
}

fn verify_argument(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    param: &PhysicalParam,
    semantic: &hew_sir::SemAbiParam,
    source: StorageId,
) -> Result<(), PhysicalError> {
    let expected_carrier = if semantic.passing == SemParamPassing::BorrowMut
        || matches!(param.layout.repr, PhysicalRepr::Struct(_))
    {
        ParamCarrier::Indirect
    } else {
        ParamCarrier::Direct
    };
    if param.ty != semantic.ty
        || param.passing != semantic.passing
        || module.target.layout(&param.ty) != Some(&param.layout)
        || param.carrier != expected_carrier
        || storage(function, source)?.ty != param.ty
    {
        return Err(PhysicalError::new(
            "indirect argument differs from its exact ABI",
        ));
    }
    Ok(())
}

pub(super) fn depends_on(function: &PhysicalFunction, mut id: StorageId, owner: StorageId) -> bool {
    for _ in 0..=function.storage.len() {
        if id == owner {
            return true;
        }
        let Some(slot) = function.storage.get(id.0 as usize) else {
            return false;
        };
        if let Some(parent) = slot.borrow_parent {
            id = parent;
        } else if let StorageOrigin::Capture { environment, .. } = slot.origin {
            id = environment;
        } else if let Some(projection) = function
            .place_storage
            .get(&id)
            .filter(|projection| projection.root != id)
        {
            id = projection.root;
        } else {
            return false;
        }
    }
    // A cycle cannot prove independence from an owner.
    true
}

pub(super) fn invalidate_captures(
    function: &PhysicalFunction,
    state: &mut FlowState,
    environment: StorageId,
) {
    for slot in &function.storage {
        if matches!(slot.origin, StorageOrigin::Capture { environment: owner, .. } if owner == environment)
        {
            state.slots[slot.id.0 as usize] = InitState::Uninitialized;
        }
    }
}

pub(super) fn verify_capture_return(
    function: &PhysicalFunction,
    state: &FlowState,
) -> Result<(), PhysicalError> {
    for slot in &function.storage {
        if let StorageOrigin::Capture { environment, .. } = slot.origin {
            if storage(function, environment)?.own == OwnKind::Guaranteed
                && state.slots[slot.id.0 as usize] != InitState::Initialized
            {
                return Err(PhysicalError::new(
                    "borrowed closure return must restore every capture",
                ));
            }
        }
    }
    Ok(())
}

impl FunctionLowerer<'_> {
    /// Realize one semantic signature's argument ABI.
    fn call_params(
        &self,
        signature: &hew_sir::SemSignature,
    ) -> Result<Vec<PhysicalParam>, PhysicalError> {
        signature
            .params
            .iter()
            .map(|param| {
                let layout = required_layout(self.target, &param.ty)?.clone();
                Ok(PhysicalParam {
                    ty: param.ty.clone(),
                    passing: param.passing,
                    carrier: param_carrier(param.passing, &layout),
                    layout,
                })
            })
            .collect()
    }

    /// Realize a vtable dispatch: the receiver rides a pointer into the
    /// erased box, and the rest of the ABI is the slot's own signature.
    pub(super) fn lower_dyn_call(
        &self,
        call: &SemTerminator,
    ) -> Result<PhysicalTerminator, PhysicalError> {
        let SemTerminator::DynCall {
            receiver,
            slot,
            signature,
            args,
            result,
            normal,
            unwind,
            ..
        } = call
        else {
            unreachable!()
        };
        Ok(PhysicalTerminator::DynCall {
            receiver: self.argument_transfer(receiver.operand.value, receiver.decision)?,
            slot: *slot,
            signature: PhysicalCallSignature {
                params: self.call_params(signature)?,
                return_ty: signature.return_ty.clone(),
                return_layout: if matches!(
                    signature.return_ty,
                    ResolvedTy::Unit | ResolvedTy::Never
                ) {
                    None
                } else {
                    Some(required_layout(self.target, &signature.return_ty)?.clone())
                },
            },
            args: self.argument_transfers(args)?,
            result: match result {
                CallResult::Unit | CallResult::Never => None,
                CallResult::Value(value) => Some(self.value(value.id)?),
            },
            normal: normal
                .as_ref()
                .map(|edge| self.lower_edge(edge))
                .transpose()?,
            unwind: match unwind {
                CallUnwind::NotApplicable => None,
                CallUnwind::Cleanup(edge) => Some(self.lower_edge(edge)?),
            },
        })
    }

    pub(super) fn lower_indirect_call(
        &self,
        call: &SemTerminator,
    ) -> Result<PhysicalTerminator, PhysicalError> {
        let SemTerminator::IndirectCall {
            callee,
            signature,
            args,
            result,
            normal,
            unwind,
            ..
        } = call
        else {
            unreachable!()
        };

        let params = self.call_params(signature)?;
        Ok(PhysicalTerminator::IndirectCall {
            callee: self.argument_transfer(callee.operand.value, callee.decision)?,
            signature: PhysicalCallSignature {
                params,
                return_ty: signature.return_ty.clone(),
                return_layout: if matches!(
                    signature.return_ty,
                    ResolvedTy::Unit | ResolvedTy::Never
                ) {
                    None
                } else {
                    Some(required_layout(self.target, &signature.return_ty)?.clone())
                },
            },
            args: self.argument_transfers(args)?,
            result: match result {
                CallResult::Unit | CallResult::Never => None,
                CallResult::Value(value) => Some(self.value(value.id)?),
            },
            normal: normal
                .as_ref()
                .map(|edge| self.lower_edge(edge))
                .transpose()?,
            unwind: match unwind {
                CallUnwind::NotApplicable => None,
                CallUnwind::Cleanup(edge) => Some(self.lower_edge(edge)?),
            },
        })
    }
}

pub(super) fn verify_callable_coerce(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    dest: StorageId,
    source: StorageId,
) -> Result<(), PhysicalError> {
    let source = storage(function, source)?;
    let dest = storage(function, dest)?;
    if source.own != OwnKind::Owned || dest.own != OwnKind::Owned {
        return Err(PhysicalError::new(
            "callable coercion must transfer an owned carrier",
        ));
    }
    hew_sir::verify_callable_coercion(&source.ty, &dest.ty, &module.type_facts)
        .map_err(PhysicalError::new)?;

    Ok(())
}

/// The erasure's storage agrees with the table it names on both sides.
fn verify_dyn_make(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    dest: StorageId,
    vtable: super::PhysicalVtableId,
    source: StorageId,
) -> Result<(), PhysicalError> {
    let table = module
        .vtables
        .get(vtable.0 as usize)
        .filter(|table| table.id == vtable)
        .ok_or_else(|| PhysicalError::new("erasure names no realized dispatch table"))?;
    let dest = storage(function, dest)?;
    let source = storage(function, source)?;
    if dest.ty != table.dyn_ty
        || dest.own != OwnKind::Owned
        || source.ty != table.concrete_ty
        || source.own != OwnKind::Owned
    {
        return Err(PhysicalError::new(
            "physical erasure disagrees with its dispatch table's types or ownership",
        ));
    }
    Ok(())
}

/// The erased dispatch boundary agrees with every table that reaches it.
pub(super) fn verify_dyn_call(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    receiver: ArgumentTransfer,
    slot: u32,
    signature: &PhysicalCallSignature,
    args: &[ArgumentTransfer],
    result: Option<StorageId>,
) -> Result<(), PhysicalError> {
    let source = match receiver {
        ArgumentTransfer::Borrow(id)
        | ArgumentTransfer::BorrowMut(id)
        | ArgumentTransfer::Move(id)
        | ArgumentTransfer::Clone { source: id, .. } => id,
    };
    let receiver_storage = storage(function, source)?;
    if !matches!(receiver_storage.ty, ResolvedTy::TraitObject { .. }) {
        return Err(PhysicalError::new(
            "physical dynamic dispatch requires a trait-object receiver",
        ));
    }
    let mut reached = 0usize;
    for table in module
        .vtables
        .iter()
        .filter(|table| table.dyn_ty == receiver_storage.ty)
    {
        reached += 1;
        let published = table
            .slots
            .iter()
            .find(|published| published.slot == slot)
            .ok_or_else(|| {
                PhysicalError::new(format!(
                    "realized table for `{}` publishes no slot {slot}",
                    table.concrete_ty.user_facing()
                ))
            })?;
        if &published.signature != signature {
            return Err(PhysicalError::new(format!(
                "physical dispatch of slot {slot} differs from the ABI `{}` was erased under",
                table.concrete_ty.user_facing()
            )));
        }
    }
    if reached == 0 {
        return Err(PhysicalError::new(
            "physical dynamic dispatch has no realized dispatch table",
        ));
    }
    if args.len() != signature.params.len() {
        return Err(PhysicalError::new(
            "physical dynamic dispatch argument count differs from its signature",
        ));
    }
    match (result, signature.return_layout.as_ref()) {
        (None, None) => Ok(()),
        (Some(result), Some(layout)) => {
            let result = storage(function, result)?;
            if &result.layout != layout || result.ty != signature.return_ty {
                return Err(PhysicalError::new(
                    "physical dynamic dispatch result differs from its signature",
                ));
            }
            Ok(())
        }
        _ => Err(PhysicalError::new(
            "physical dynamic dispatch result presence differs from its signature",
        )),
    }
}

pub(super) fn verify_operation(
    module: &PhysicalModule,
    function: &PhysicalFunction,
    operation: &PhysicalOp,
) -> Result<(), PhysicalError> {
    match operation {
        PhysicalOp::FunctionMake { dest, callee } => {
            verify_function_make(module, function, *dest, *callee)
        }
        PhysicalOp::ClosureMake {
            dest,
            closure,
            fields,
        } => verify_closure_make(module, function, *dest, *closure, fields),
        PhysicalOp::CallableCoerce { dest, source } => {
            verify_callable_coerce(module, function, *dest, *source)
        }
        PhysicalOp::DynMake {
            dest,
            vtable,
            source,
        } => verify_dyn_make(module, function, *dest, *vtable, *source),
        _ => unreachable!("only callable construction operations are dispatched"),
    }
}
