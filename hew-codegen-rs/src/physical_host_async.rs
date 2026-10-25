//! Typed asynchronous presentations of the existing checked host call boundary.

use super::*;

struct CallStorage<'ctx> {
    layout: inkwell::types::StructType<'ctx>,
    arguments: inkwell::types::StructType<'ctx>,
    inputs: inkwell::types::StructType<'ctx>,
    consumed: usize,
    size: u64,
    align: u64,
}

impl<'ctx> CallStorage<'ctx> {
    fn new(emitter: &ModuleEmitter<'ctx, '_>, body: &PhysicalCallable) -> CodegenResult<Self> {
        let pointer = emitter.ctx.ptr_type(AddressSpace::default());
        let args = body
            .params
            .iter()
            .map(|param| {
                if param.carrier == ParamCarrier::Indirect {
                    Ok(pointer.into())
                } else {
                    llvm_type(emitter.ctx, &param.layout.repr)
                }
            })
            .collect::<CodegenResult<Vec<_>>>()?;
        let arguments = emitter.ctx.struct_type(&args, false);
        let inputs = emitter
            .ctx
            .struct_type(&public_input_types(emitter, body)?, false);
        let consumed = body
            .params
            .iter()
            .filter(|param| {
                opaque(&param.ty) && param.passing == hew_mir::physical::SemParamPassing::Consume
            })
            .count();
        let result = body.return_layout.as_ref().map_or_else(
            || Ok(emitter.ctx.i8_type().into()),
            |layout| llvm_type(emitter.ctx, &layout.repr),
        )?;
        let layout = emitter.ctx.struct_type(
            &[
                arguments.into(),
                inputs.into(),
                pointer.array_type(consumed as u32).into(),
                result,
                pointer.into(),
                pointer.into(),
            ],
            false,
        );
        let target = TargetData::create(&emitter.module.target.data_layout);
        Ok(Self {
            layout,
            arguments,
            inputs,
            consumed,
            size: target.get_abi_size(&layout).max(1),
            align: u64::from(target.get_abi_alignment(&layout)),
        })
    }

    fn field(
        &self,
        builder: &Builder<'ctx>,
        owner: PointerValue<'ctx>,
        index: u32,
    ) -> CodegenResult<PointerValue<'ctx>> {
        builder
            .build_struct_gep(self.layout, owner, index, "call.field")
            .llvm_ctx("address retained host storage")
    }
}

pub(super) fn emit<'ctx>(
    emitter: &ModuleEmitter<'ctx, '_>,
    export: &ExportedFunction,
) -> CodegenResult<()> {
    let body = callable(emitter.module, export.callable)?;
    let storage = CallStorage::new(emitter, body)?;
    let finish = emit_finish(emitter, export, body, &storage)?;
    let free = emit_free(emitter, export, &storage)?;
    emit_start(emitter, export, body, &storage, finish, free)?;
    emit_result(emitter, export, body, &storage)?;
    let pointer = emitter.ctx.ptr_type(AddressSpace::default());
    for (suffix, runtime, count, returns) in [
        ("poll", "hew_host_call_poll", 1, true),
        ("cancel", "hew_host_call_cancel", 1, false),
        ("release", "hew_host_call_release", 2, true),
    ] {
        let params = vec![pointer.into(); count];
        let ty = if returns {
            emitter.ctx.i32_type().fn_type(&params, false)
        } else {
            emitter.ctx.void_type().fn_type(&params, false)
        };
        let wrapper = emitter.llvm.add_function(
            &format!("{}_{suffix}", export.symbol),
            ty,
            Some(Linkage::External),
        );
        let target = get_or_declare_external(&emitter.llvm, runtime, ty)?;
        let builder = emitter.ctx.create_builder();
        builder.position_at_end(emitter.ctx.append_basic_block(wrapper, "entry"));
        let arguments = wrapper
            .get_params()
            .into_iter()
            .map(Into::into)
            .collect::<Vec<_>>();
        let call = builder
            .build_call(target, &arguments, "")
            .llvm_ctx("invoke host call operation")?;
        if returns {
            builder
                .build_return(Some(&call.try_as_basic_value().basic().unwrap()))
                .llvm_ctx("return host call status")?;
        } else {
            builder
                .build_return(None)
                .llvm_ctx("return host cancellation request")?;
        }
    }
    Ok(())
}

fn emit_finish<'ctx>(
    emitter: &ModuleEmitter<'ctx, '_>,
    export: &ExportedFunction,
    body: &PhysicalCallable,
    storage: &CallStorage<'ctx>,
) -> CodegenResult<FunctionValue<'ctx>> {
    let pointer = emitter.ctx.ptr_type(AddressSpace::default());
    let finish = emitter.llvm.add_function(
        &format!("{}$finish", export.symbol),
        emitter
            .ctx
            .void_type()
            .fn_type(&[pointer.into(), emitter.ctx.i32_type().into()], false),
        Some(Linkage::Internal),
    );
    let builder = emitter.ctx.create_builder();
    builder.position_at_end(emitter.ctx.append_basic_block(finish, "entry"));
    let owner = finish.get_first_param().unwrap().into_pointer_value();
    let inputs = storage.field(&builder, owner, 1)?;
    let mut values = Vec::new();
    for (index, ty) in storage.inputs.get_field_types().into_iter().enumerate() {
        let slot = builder
            .build_struct_gep(storage.inputs, inputs, index as u32, "input")
            .llvm_ctx("address borrowed host input")?;
        values.push(
            builder
                .build_load(ty, slot, "input.value")
                .llvm_ctx("load retained host input")?,
        );
    }
    let owners = storage.field(&builder, owner, 2)?;
    let mut consumed = Vec::new();
    for index in 0..storage.consumed {
        // SAFETY: the checked consumed-owner count is the exact array extent.
        let slot = unsafe {
            builder.build_gep(
                pointer,
                owners,
                &[emitter.ctx.i64_type().const_int(index as u64, false)],
                "owner",
            )
        }
        .llvm_ctx("address consumed host owner")?;
        consumed.push(
            builder
                .build_load(pointer, slot, "owner.value")
                .llvm_ctx("load consumed host owner")?
                .into_pointer_value(),
        );
    }
    finish_arguments(emitter, &builder, &values, body, &consumed)?;
    builder
        .build_return(None)
        .llvm_ctx("finish host input handoff")?;
    Ok(finish)
}

fn emit_free<'ctx>(
    emitter: &ModuleEmitter<'ctx, '_>,
    export: &ExportedFunction,
    storage: &CallStorage<'ctx>,
) -> CodegenResult<FunctionValue<'ctx>> {
    let pointer = emitter.ctx.ptr_type(AddressSpace::default());
    let free = emitter.llvm.add_function(
        &format!("{}$free", export.symbol),
        emitter.ctx.void_type().fn_type(&[pointer.into()], false),
        Some(Linkage::Internal),
    );
    let builder = emitter.ctx.create_builder();
    builder.position_at_end(emitter.ctx.append_basic_block(free, "entry"));
    let release = get_or_declare_external(
        &emitter.llvm,
        "hew_dealloc",
        emitter.ctx.void_type().fn_type(
            &[
                pointer.into(),
                emitter.ctx.i64_type().into(),
                emitter.ctx.i64_type().into(),
            ],
            false,
        ),
    )?;
    builder
        .build_call(
            release,
            &[
                free.get_first_param().unwrap().into(),
                emitter.ctx.i64_type().const_int(storage.size, false).into(),
                emitter
                    .ctx
                    .i64_type()
                    .const_int(storage.align, false)
                    .into(),
            ],
            "",
        )
        .llvm_ctx("release drained host storage")?;
    builder
        .build_return(None)
        .llvm_ctx("finish host storage release")?;
    Ok(free)
}

fn emit_start<'ctx>(
    emitter: &ModuleEmitter<'ctx, '_>,
    export: &ExportedFunction,
    body: &PhysicalCallable,
    storage: &CallStorage<'ctx>,
    finish: FunctionValue<'ctx>,
    free: FunctionValue<'ctx>,
) -> CodegenResult<()> {
    let ctx = emitter.ctx;
    let pointer = ctx.ptr_type(AddressSpace::default());
    let inputs = public_input_types(emitter, body)?;
    let guest = emitter.module.target.triple.starts_with("wasm");
    let offset = usize::from(!guest);
    let mut params: Vec<BasicMetadataTypeEnum<'ctx>> = vec![pointer.into(); offset];
    params.extend(inputs.iter().copied().map(BasicMetadataTypeEnum::from));
    params.extend([BasicMetadataTypeEnum::from(pointer); 3]);
    let wrapper = emitter.llvm.add_function(
        &format!("{}_start", export.symbol),
        ctx.i32_type().fn_type(&params, false),
        Some(Linkage::External),
    );
    let builder = ctx.create_builder();
    builder.position_at_end(ctx.append_basic_block(wrapper, "entry"));
    let arguments = wrapper.get_params();
    let output = arguments[inputs.len() + offset + 1].into_pointer_value();
    let error = arguments[inputs.len() + offset + 2].into_pointer_value();
    builder
        .build_store(output, pointer.const_null())
        .llvm_ctx("initialize host call owner")?;
    let prepare = get_or_declare_external(
        &emitter.llvm,
        if guest {
            "hew_host_call_new_guest"
        } else {
            "hew_host_call_new"
        },
        pointer.fn_type(&vec![pointer.into(); offset + 2], false),
    )?;
    let mut admission = Vec::new();
    if !guest {
        admission.push(arguments[0].into());
    }
    admission.push(arguments[inputs.len() + offset].into());
    admission.push(error.into());
    let call = suspend::call_value(&builder, prepare, &admission, "call")?.into_pointer_value();
    let ready = ctx.append_basic_block(wrapper, "prepared");
    let refused = ctx.append_basic_block(wrapper, "refused");
    let valid = builder
        .build_is_not_null(call, "prepared")
        .llvm_ctx("inspect host runtime admission")?;
    builder
        .build_conditional_branch(valid, ready, refused)
        .llvm_ctx("guard consuming host inputs")?;
    builder.position_at_end(refused);
    let error_owner = builder
        .build_load(pointer, error, "error")
        .llvm_ctx("load host admission error")?
        .into_pointer_value();
    let status = call_pointer_status(emitter, &builder, "hew_host_error_code", error_owner)?;
    builder
        .build_return(Some(&status))
        .llvm_ctx("return host admission refusal")?;
    builder.position_at_end(ready);
    let allocate = get_or_declare_external(
        &emitter.llvm,
        "hew_alloc",
        pointer.fn_type(&[ctx.i64_type().into(); 2], false),
    )?;
    let owner = suspend::call_value(
        &builder,
        allocate,
        &[
            ctx.i64_type().const_int(storage.size, false).into(),
            ctx.i64_type().const_int(storage.align, false).into(),
        ],
        "call.storage",
    )?
    .into_pointer_value();
    let public = &arguments[offset..inputs.len() + offset];
    let input_storage = storage.field(&builder, owner, 1)?;
    for (index, value) in public.iter().enumerate() {
        let slot = builder
            .build_struct_gep(storage.inputs, input_storage, index as u32, "input")
            .llvm_ctx("address retained host input")?;
        builder
            .build_store(slot, *value)
            .llvm_ctx("retain host input carrier")?;
    }
    let (values, consumed) = input_arguments(emitter, &builder, public, body, Some(owner))?;
    let argument_storage = storage.field(&builder, owner, 0)?;
    for (index, value) in values.iter().enumerate() {
        let slot = builder
            .build_struct_gep(
                storage.arguments,
                argument_storage,
                index as u32,
                "argument",
            )
            .llvm_ctx("address checked host carrier")?;
        builder
            .build_store(slot, *value)
            .llvm_ctx("retain checked host carrier")?;
    }
    let owners = storage.field(&builder, owner, 2)?;
    for (index, value) in consumed.iter().enumerate() {
        // SAFETY: input lowering returns one owner for each checked consuming parameter.
        let slot = unsafe {
            builder.build_gep(
                pointer,
                owners,
                &[ctx.i64_type().const_int(index as u64, false)],
                "owner",
            )
        }
        .llvm_ctx("address transferred host owner")?;
        builder
            .build_store(slot, *value)
            .llvm_ctx("retain transferred host allocation")?;
    }
    let (result, result_owner) = result_storage(
        emitter,
        &builder,
        body,
        &export.symbol,
        Some(storage.field(&builder, owner, 3)?),
    )?;
    builder
        .build_store(
            storage.field(&builder, owner, 4)?,
            result_owner.unwrap_or(pointer.const_null()),
        )
        .llvm_ctx("retain host result owner")?;
    let fault = storage.field(&builder, owner, 5)?;
    builder
        .build_store(fault, pointer.const_null())
        .llvm_ctx("initialize host fault storage")?;
    let start = emitter.emit_root_start(body)?;
    let invoke = get_or_declare_external(
        &emitter.llvm,
        "hew_host_call_start",
        ctx.void_type().fn_type(&[pointer.into(); 7], false),
    )?;
    builder
        .build_call(
            invoke,
            &[
                call.into(),
                start.as_global_value().as_pointer_value().into(),
                owner.into(),
                result.into(),
                fault.into(),
                finish.as_global_value().as_pointer_value().into(),
                free.as_global_value().as_pointer_value().into(),
            ],
            "",
        )
        .llvm_ctx("start checked asynchronous host call")?;
    builder
        .build_store(output, call)
        .llvm_ctx("publish host call owner")?;
    builder
        .build_return(Some(&ctx.i32_type().const_zero()))
        .llvm_ctx("return host call admission")?;
    Ok(())
}

fn call_pointer_status<'ctx>(
    emitter: &ModuleEmitter<'ctx, '_>,
    builder: &Builder<'ctx>,
    name: &str,
    input: PointerValue<'ctx>,
) -> CodegenResult<IntValue<'ctx>> {
    let target = get_or_declare_external(
        &emitter.llvm,
        name,
        emitter
            .ctx
            .i32_type()
            .fn_type(&[input.get_type().into()], false),
    )?;
    Ok(suspend::call_value(builder, target, &[input.into()], "status")?.into_int_value())
}

fn emit_result<'ctx>(
    emitter: &ModuleEmitter<'ctx, '_>,
    export: &ExportedFunction,
    body: &PhysicalCallable,
    storage: &CallStorage<'ctx>,
) -> CodegenResult<()> {
    let ctx = emitter.ctx;
    let pointer = ctx.ptr_type(AddressSpace::default());
    let has_result = body.return_ty != ResolvedTy::Unit;
    let wrapper = emitter.llvm.add_function(
        &format!("{}_result", export.symbol),
        ctx.i32_type()
            .fn_type(&vec![pointer.into(); 2 + usize::from(has_result)], false),
        Some(Linkage::External),
    );
    let builder = ctx.create_builder();
    builder.position_at_end(ctx.append_basic_block(wrapper, "entry"));
    let call = wrapper.get_first_param().unwrap().into_pointer_value();
    let error = wrapper.get_last_param().unwrap().into_pointer_value();
    let reserve = get_or_declare_external(
        &emitter.llvm,
        "hew_host_call_result_begin",
        ctx.i32_type().fn_type(&[pointer.into(); 2], false),
    )?;
    let admitted =
        suspend::call_value(&builder, reserve, &[call.into(), error.into()], "reserved")?
            .into_int_value();
    let ready = ctx.append_basic_block(wrapper, "ready");
    let refused = ctx.append_basic_block(wrapper, "refused");
    let ok = builder
        .build_int_compare(
            IntPredicate::EQ,
            admitted,
            ctx.i32_type().const_zero(),
            "reserved",
        )
        .llvm_ctx("inspect one-shot result reservation")?;
    builder
        .build_conditional_branch(ok, ready, refused)
        .llvm_ctx("guard host result transfer")?;
    builder.position_at_end(refused);
    builder
        .build_return(Some(&admitted))
        .llvm_ctx("return host result refusal")?;
    builder.position_at_end(ready);
    let owner = call_pointer(emitter, &builder, "hew_host_call_arguments", call)?;
    let status = call_pointer_status(emitter, &builder, "hew_host_call_status", call)?;
    let result_owner = if opaque(&body.return_ty) {
        Some(
            builder
                .build_load(pointer, storage.field(&builder, owner, 4)?, "result.owner")
                .llvm_ctx("load completed host result owner")?
                .into_pointer_value(),
        )
    } else {
        None
    };
    let success = ctx.append_basic_block(wrapper, "success");
    let failure = ctx.append_basic_block(wrapper, "failure");
    let ok = builder
        .build_int_compare(
            IntPredicate::EQ,
            status,
            ctx.i32_type().const_zero(),
            "success",
        )
        .llvm_ctx("inspect checked host outcome")?;
    builder
        .build_conditional_branch(ok, success, failure)
        .llvm_ctx("select terminal host result")?;
    builder.position_at_end(success);
    if has_result {
        let result = if let Some(result_owner) = result_owner {
            call_pointer(emitter, &builder, "hew_host_value_storage", result_owner)?
        } else {
            storage.field(&builder, owner, 3)?
        };
        publish_result(
            emitter,
            &builder,
            body,
            wrapper.get_nth_param(1).unwrap().into_pointer_value(),
            result,
            result_owner,
        )?;
    }
    builder
        .build_return(Some(&ctx.i32_type().const_zero()))
        .llvm_ctx("return asynchronous host result")?;
    builder.position_at_end(failure);
    if let Some(result_owner) = result_owner {
        call_forget(emitter, &builder, result_owner)?;
    }
    let code = publish_failure(emitter, &builder, error, storage.field(&builder, owner, 5)?)?;
    builder
        .build_return(Some(&code))
        .llvm_ctx("return asynchronous host failure")?;
    Ok(())
}
