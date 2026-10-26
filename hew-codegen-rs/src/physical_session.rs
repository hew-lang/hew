//! Native realization of compiler-owned persistent session places.

use super::*;

pub(super) fn context_argument<'ctx>(
    function: &PhysicalFunction,
    value: FunctionValue<'ctx>,
    context: StorageId,
) -> CodegenResult<PointerValue<'ctx>> {
    let index = function
        .parameters
        .iter()
        .position(|id| *id == context)
        .ok_or_else(|| {
            CodegenError::FailClosed("session context is not an entry parameter".into())
        })?;
    value
        .get_nth_param(
            u32::try_from(index).map_err(|_| {
                CodegenError::FailClosed("session parameter index exceeds u32".into())
            })?,
        )
        .map(BasicValueEnum::into_pointer_value)
        .ok_or_else(|| CodegenError::FailClosed("session context parameter is absent".into()))
}

pub(super) fn allocate_slot<'ctx>(
    module: &ModuleEmitter<'ctx, '_>,
    function: &PhysicalFunction,
    value: FunctionValue<'ctx>,
    builder: &Builder<'ctx>,
    storage: &PhysicalStorage,
) -> CodegenResult<PointerValue<'ctx>> {
    let hew_mir::physical::StorageOrigin::Session {
        context,
        slot,
        ref recipe,
        ..
    } = storage.origin
    else {
        unreachable!()
    };
    let descriptor = format!("__hew_session_{}_{}", function.callable.0, slot);
    module.emit_value_descriptor(&descriptor, recipe)?;
    let pointer = module.ctx.ptr_type(AddressSpace::default());
    let usize_ty = module
        .ctx
        .ptr_sized_int_type(&TargetData::create(&module.module.target.data_layout), None);
    let prepare = get_or_declare_external(
        &module.llvm,
        "hew_live_slot_storage",
        pointer.fn_type(&[pointer.into(), usize_ty.into(), pointer.into()], false),
    )?;
    Ok(suspend::call_value(
        builder,
        prepare,
        &[
            context_argument(function, value, context)?.into(),
            usize_ty.const_int(u64::from(slot), false).into(),
            module
                .llvm
                .get_global(&descriptor)
                .unwrap()
                .as_pointer_value()
                .into(),
        ],
        "session.storage",
    )?
    .into_pointer_value())
}

pub(super) fn initialized<'ctx>(
    module: &ModuleEmitter<'ctx, '_>,
    builder: &Builder<'ctx>,
    context: PointerValue<'ctx>,
    slot: u32,
) -> CodegenResult<IntValue<'ctx>> {
    let pointer = module.ctx.ptr_type(AddressSpace::default());
    let usize_ty = module
        .ctx
        .ptr_sized_int_type(&TargetData::create(&module.module.target.data_layout), None);
    let query = get_or_declare_external(
        &module.llvm,
        "hew_live_slot_initialized",
        module
            .ctx
            .bool_type()
            .fn_type(&[pointer.into(), usize_ty.into()], false),
    )?;
    Ok(suspend::call_value(
        builder,
        query,
        &[
            context.into(),
            usize_ty.const_int(u64::from(slot), false).into(),
        ],
        "session.initialized",
    )?
    .into_int_value())
}

impl FunctionEmitter<'_, '_> {
    pub(super) fn publish_session_initialization(
        &self,
        root: StorageId,
        initialized: bool,
    ) -> CodegenResult<()> {
        let hew_mir::physical::StorageOrigin::Session { context, slot, .. } =
            self.storage(root)?.origin
        else {
            return Ok(());
        };
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let usize_ty = self
            .ctx
            .ptr_sized_int_type(&TargetData::create(&self.module.target.data_layout), None);
        let publish = get_or_declare_external(
            self.llvm,
            "hew_live_slot_set_initialized",
            self.ctx.void_type().fn_type(
                &[pointer.into(), usize_ty.into(), self.ctx.bool_type().into()],
                false,
            ),
        )?;
        self.builder
            .build_call(
                publish,
                &[
                    self.load(context, "session.context")?.into(),
                    usize_ty.const_int(u64::from(slot), false).into(),
                    self.ctx
                        .bool_type()
                        .const_int(u64::from(initialized), false)
                        .into(),
                ],
                "",
            )
            .llvm_ctx("publish session binding ownership")?;
        Ok(())
    }
}
