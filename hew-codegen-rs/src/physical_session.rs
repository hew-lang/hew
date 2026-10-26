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

/// Register MIR's exact content partition and borrow its persistent leaf bits.
pub(super) fn register_leaves<'ctx>(
    module: &ModuleEmitter<'ctx, '_>,
    function: &PhysicalFunction,
    value: FunctionValue<'ctx>,
    builder: &Builder<'ctx>,
    slots: &[PointerValue<'ctx>],
    storage: &PhysicalStorage,
) -> CodegenResult<Vec<(StorageId, PointerValue<'ctx>)>> {
    let hew_mir::physical::StorageOrigin::Session { context, slot, .. } = storage.origin else {
        unreachable!()
    };
    let partition = function.place_storage.get(&storage.id).ok_or_else(|| {
        CodegenError::FailClosed("session place lacks its canonical leaf partition".into())
    })?;
    let pointer = module.ctx.ptr_type(AddressSpace::default());
    let usize_ty = module
        .ctx
        .ptr_sized_int_type(&TargetData::create(&module.module.target.data_layout), None);
    let leaf_ty = module.ctx.struct_type(
        &[
            pointer.into(),
            pointer.into(),
            pointer.into(),
            usize_ty.into(),
        ],
        false,
    );
    let count = u32::try_from(partition.leaves.len())
        .map_err(|_| CodegenError::FailClosed("session leaf count exceeds u32".into()))?;
    let table_ty = leaf_ty.array_type(count);
    let table = super::coro::entry_storage(
        module.ctx,
        value.get_first_basic_block().unwrap(),
        table_ty.into(),
        "session.leaves",
    )?;
    for (index, leaf) in partition.leaves.iter().enumerate() {
        let index = u32::try_from(index).unwrap();
        let descriptor = format!("__hew_session_{}_{}_leaf{index}", function.callable.0, slot);
        module.emit_value_descriptor(&descriptor, &leaf.recipe)?;
        let path = &function.place_storage[&leaf.storage].path;
        let path_values: Vec<_> = path
            .iter()
            .map(|step| {
                module
                    .ctx
                    .i32_type()
                    .const_int(u64::from(step.field), false)
            })
            .collect();
        let path_value = module.ctx.i32_type().const_array(&path_values);
        let path_global =
            module
                .llvm
                .add_global(path_value.get_type(), None, &format!("{descriptor}_path"));
        path_global.set_linkage(inkwell::module::Linkage::Internal);
        path_global.set_constant(true);
        path_global.set_initializer(&path_value);
        // SAFETY: index is bounded by the same canonical leaf count used for table_ty.
        let entry = unsafe {
            builder.build_in_bounds_gep(
                table_ty,
                table,
                &[
                    module.ctx.i32_type().const_zero(),
                    module.ctx.i32_type().const_int(u64::from(index), false),
                ],
                "session.leaf",
            )
        }
        .llvm_ctx("address session leaf descriptor")?;
        let fields: [BasicValueEnum<'ctx>; 4] = [
            slots[leaf.storage.0 as usize].into(),
            module
                .llvm
                .get_global(&descriptor)
                .unwrap()
                .as_pointer_value()
                .into(),
            path_global.as_pointer_value().into(),
            usize_ty.const_int(path.len() as u64, false).into(),
        ];
        for (field, field_value) in fields.into_iter().enumerate() {
            let field_slot = builder
                .build_struct_gep(leaf_ty, entry, field as u32, "session.leaf.field")
                .llvm_ctx("address session leaf field")?;
            builder
                .build_store(field_slot, field_value)
                .llvm_ctx("initialize session leaf descriptor")?;
        }
    }
    let register = get_or_declare_external(
        &module.llvm,
        "hew_live_slot_register",
        pointer.fn_type(
            &[
                pointer.into(),
                usize_ty.into(),
                pointer.into(),
                usize_ty.into(),
            ],
            false,
        ),
    )?;
    let flags = suspend::call_value(
        builder,
        register,
        &[
            context_argument(function, value, context)?.into(),
            usize_ty.const_int(u64::from(slot), false).into(),
            table.into(),
            usize_ty.const_int(u64::from(count), false).into(),
        ],
        "session.flags",
    )?
    .into_pointer_value();
    partition
        .leaves
        .iter()
        .enumerate()
        .map(|(index, leaf)| {
            // SAFETY: registration returns one persistent byte for each canonical leaf.
            let flag = unsafe {
                builder.build_in_bounds_gep(
                    module.ctx.i8_type(),
                    flags,
                    &[usize_ty.const_int(index as u64, false)],
                    "session.leaf.initialized",
                )
            }
            .llvm_ctx("address persistent session leaf bit")?;
            Ok((leaf.storage, flag))
        })
        .collect()
}

impl FunctionEmitter<'_, '_> {
    pub(super) fn publish_session_change(&self, root: StorageId) -> CodegenResult<()> {
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
            "hew_live_slot_changed",
            self.ctx
                .void_type()
                .fn_type(&[pointer.into(), usize_ty.into()], false),
        )?;
        self.builder
            .build_call(
                publish,
                &[
                    self.load(context, "session.context")?.into(),
                    usize_ty.const_int(u64::from(slot), false).into(),
                ],
                "",
            )
            .llvm_ctx("publish session binding ownership")?;
        Ok(())
    }
}
