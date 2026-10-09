use super::*;
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::values::{AsValueRef, InstructionOpcode};

struct Layout {
    size: u64,
}

impl Layout {
    fn field(
        &mut self,
        target: &TargetData,
        ty: BasicTypeEnum<'_>,
        align: u32,
    ) -> CodegenResult<u64> {
        let align = u64::from(align.max(target.get_abi_alignment(&ty)));
        if align > 16 || !align.is_power_of_two() {
            return Err(CodegenError::FailClosed(
                "continuation field exceeds frame alignment".into(),
            ));
        }
        let offset = self
            .size
            .checked_add(align - 1)
            .map(|size| size & !(align - 1))
            .ok_or_else(|| CodegenError::FailClosed("continuation alignment overflows".into()))?;
        self.size = offset
            .checked_add(target.get_abi_size(&ty))
            .ok_or_else(|| CodegenError::FailClosed("continuation layout overflows".into()))?;
        Ok(offset)
    }
}

fn address<'ctx>(
    ctx: &'ctx Context,
    builder: &Builder<'ctx>,
    base: PointerValue<'ctx>,
    offset: u64,
    name: &str,
) -> CodegenResult<PointerValue<'ctx>> {
    // SAFETY: the offset names a field inside the retained frame allocation.
    unsafe {
        builder.build_in_bounds_gep(
            ctx.i8_type(),
            base,
            &[ctx.i64_type().const_int(offset, false)],
            name,
        )
    }
    .llvm_ctx("address continuation field")
}

pub(super) fn lower<'ctx>(
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    target: &TargetData,
    ramp: FunctionValue<'ctx>,
) -> CodegenResult<()> {
    let pointer = ctx.ptr_type(AddressSpace::default());
    let prefix_size = target.get_abi_size(&pointer) * 2;
    let index_offset = prefix_size;
    let destroying_offset = index_offset + 4;
    let mut layout = Layout {
        size: destroying_offset + 1,
    };
    let name = ramp.get_name().to_string_lossy();
    let body = module.add_function(
        &format!("{name}$body"),
        ctx.void_type().fn_type(&[pointer.into()], false),
        Some(Linkage::Internal),
    );
    let destroy = module.add_function(
        &format!("{name}$destroy"),
        body.get_type(),
        Some(Linkage::Internal),
    );
    let blocks = ramp.get_basic_blocks();
    let dispatch = blocks
        .iter()
        .find(|block| block.get_name().to_bytes() == b"frame.dispatch")
        .copied()
        .ok_or_else(|| CodegenError::FailClosed("continuation has no dispatch entry".into()))?;
    for block in
        std::iter::once(dispatch).chain(blocks.iter().copied().filter(|block| *block != dispatch))
    {
        block
            .remove_from_function()
            .map_err(|()| CodegenError::FailClosed("cannot move continuation block".into()))?;
        // SAFETY: the orphan block and destination function belong to this context.
        unsafe {
            inkwell::llvm_sys::core::LLVMAppendExistingBasicBlock(
                body.as_value_ref(),
                block.as_mut_ptr(),
            );
        }
    }
    let base = body.get_first_param().unwrap().into_pointer_value();
    let builder = ctx.create_builder();
    let first = dispatch
        .get_instructions()
        .find(|instruction| instruction.get_opcode() != InstructionOpcode::Alloca)
        .ok_or_else(|| CodegenError::FailClosed("continuation dispatch is empty".into()))?;
    builder.position_before(&first);
    let mut parameters = Vec::new();
    for parameter in ramp.get_params() {
        let offset = layout.field(target, parameter.get_type(), 0)?;
        let slot = address(ctx, &builder, base, offset, "frame.argument")?;
        let loaded = builder
            .build_load(parameter.get_type(), slot, "frame.parameter")
            .llvm_ctx("restore continuation argument")?;
        // SAFETY: both values have the exact ramp argument type; the ramp has no
        // instructions until its captures are emitted below.
        unsafe {
            inkwell::llvm_sys::core::LLVMReplaceAllUsesWith(
                parameter.as_value_ref(),
                loaded.as_value_ref(),
            );
        }
        parameters.push((parameter, offset));
    }
    let mut allocas = Vec::new();
    for block in body.get_basic_blocks() {
        allocas.extend(
            block
                .get_instructions()
                .filter(|instruction| instruction.get_opcode() == InstructionOpcode::Alloca),
        );
    }
    for allocation in allocas {
        allocation
            .get_operand(0)
            .and_then(|operand| operand.value())
            .and_then(|value| value.into_int_value().get_zero_extended_constant())
            .filter(|count| *count == 1)
            .ok_or_else(|| {
                CodegenError::FailClosed("continuation has a variable-sized stack carrier".into())
            })?;
        if allocation
            .get_metadata(ctx.get_kind_id("hew.stack"))
            .is_some()
        {
            let name = allocation
                .get_name()
                .map(|name| name.to_string_lossy().into_owned());
            allocation.remove_from_basic_block();
            builder.position_before(&first);
            builder.insert_instruction(&allocation, name.as_deref());
            // SAFETY: the one stack residency marker has been consumed.
            unsafe {
                inkwell::llvm_sys::core::LLVMSetMetadata(
                    allocation.as_value_ref(),
                    ctx.get_kind_id("hew.stack"),
                    std::ptr::null_mut(),
                );
            }
            continue;
        }
        let field = if allocation
            .get_metadata(ctx.get_kind_id("hew.frame.base"))
            .is_some()
        {
            base
        } else {
            // SAFETY: the instruction is an alloca with a sized, typed element.
            let ty = unsafe {
                BasicTypeEnum::new(inkwell::llvm_sys::core::LLVMGetAllocatedType(
                    allocation.as_value_ref(),
                ))
            };
            let offset = if allocation
                .get_metadata(ctx.get_kind_id("hew.frame.index"))
                .is_some()
            {
                index_offset
            } else if allocation
                .get_metadata(ctx.get_kind_id("hew.frame.destroying"))
                .is_some()
            {
                destroying_offset
            } else {
                layout.field(
                    target,
                    ty,
                    allocation
                        .get_alignment()
                        .map_err(|error| CodegenError::FailClosed(error.to_string()))?,
                )?
            };
            builder.position_before(&first);
            address(
                ctx,
                &builder,
                base,
                offset,
                &allocation.get_name().unwrap().to_string_lossy(),
            )?
        };
        // SAFETY: alloca addresses and frame fields use the same opaque pointer
        // type; the old allocation is removed after its uses are redirected.
        unsafe {
            inkwell::llvm_sys::core::LLVMReplaceAllUsesWith(
                allocation.as_value_ref(),
                field.as_value_ref(),
            );
        }
        allocation.erase_from_basic_block();
    }
    for block in body.get_basic_blocks() {
        if let Some(end) = block
            .get_terminator()
            .filter(|end| end.get_opcode() == InstructionOpcode::Return)
        {
            builder.position_before(&end);
            builder
                .build_return(None)
                .llvm_ctx("return continuation body")?;
            end.erase_from_basic_block();
        }
    }
    if let Some(subprogram) = ramp.get_subprogram() {
        body.set_subprogram(subprogram);
        for name in ["noinline", "optnone"] {
            let kind = Attribute::get_named_enum_kind_id(name);
            body.add_attribute(AttributeLoc::Function, ctx.create_enum_attribute(kind, 0));
        }
        // SAFETY: the original source attribution now belongs to its one body.
        unsafe {
            inkwell::llvm_sys::debuginfo::LLVMSetSubprogram(
                ramp.as_value_ref(),
                std::ptr::null_mut(),
            );
        }
    }
    ramp.remove_string_attribute(AttributeLoc::Function, "hew.resumable");
    let size = layout
        .size
        .checked_add(15)
        .map(|size| size & !15)
        .ok_or_else(|| CodegenError::FailClosed("continuation size overflows".into()))?;
    builder.position_at_end(ctx.append_basic_block(ramp, "entry"));
    let allocate = coro::external(
        module,
        "hew_cont_frame_alloc",
        pointer.fn_type(&[ctx.i64_type().into()], false),
    )?;
    let frame = suspend::call_value(
        &builder,
        allocate,
        &[ctx.i64_type().const_int(size, false).into()],
        "frame",
    )?
    .into_pointer_value();
    let absent = builder
        .build_is_null(frame, "frame.absent")
        .llvm_ctx("inspect continuation allocation")?;
    let initialize = ctx.append_basic_block(ramp, "frame.initialize");
    let failed = ctx.append_basic_block(ramp, "frame.allocation.failed");
    builder
        .build_conditional_branch(absent, failed, initialize)
        .llvm_ctx("admit continuation allocation")?;
    builder.position_at_end(failed);
    let abort = coro::external(module, "abort", ctx.void_type().fn_type(&[], false))?;
    builder
        .build_call(abort, &[], "")
        .llvm_ctx("refuse absent continuation allocation")?;
    builder
        .build_unreachable()
        .llvm_ctx("terminate absent continuation allocation")?;
    builder.position_at_end(initialize);
    builder
        .build_store(frame, body.as_global_value().as_pointer_value())
        .llvm_ctx("publish continuation resume entry")?;
    let destroy_slot = address(
        ctx,
        &builder,
        frame,
        target.get_abi_size(&pointer),
        "frame.destroy.entry",
    )?;
    builder
        .build_store(destroy_slot, destroy.as_global_value().as_pointer_value())
        .llvm_ctx("publish continuation destroy entry")?;
    let state = address(ctx, &builder, frame, index_offset, "frame.state")?;
    builder
        .build_store(state, ctx.i32_type().const_zero())
        .llvm_ctx("initialize frame state")?;
    let destroying = address(ctx, &builder, frame, destroying_offset, "frame.destroying")?;
    builder
        .build_store(destroying, ctx.bool_type().const_zero())
        .llvm_ctx("initialize frame entry kind")?;
    for (parameter, offset) in parameters {
        let slot = address(ctx, &builder, frame, offset, "frame.capture")?;
        builder
            .build_store(slot, parameter)
            .llvm_ctx("capture continuation argument")?;
    }
    builder
        .build_call(body, &[frame.into()], "")
        .llvm_ctx("run continuation to first suspend")?;
    builder
        .build_return(Some(&frame))
        .llvm_ctx("transfer continuation to its driver")?;
    builder.position_at_end(ctx.append_basic_block(destroy, "entry"));
    let frame = destroy.get_first_param().unwrap().into_pointer_value();
    let destroying = address(ctx, &builder, frame, destroying_offset, "frame.destroying")?;
    builder
        .build_store(destroying, ctx.bool_type().const_int(1, false))
        .llvm_ctx("enter synchronous destruction")?;
    builder
        .build_call(body, &[frame.into()], "")
        .llvm_ctx("destroy continuation through its cleanup entry")?;
    builder
        .build_return(None)
        .llvm_ctx("finish synchronous continuation destruction")?;
    Ok(())
}
