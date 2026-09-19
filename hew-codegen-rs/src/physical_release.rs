//! Consuming release continuations from the ordinary physical value recipes.

use super::*;

fn symbol(action: DestroyAction) -> String {
    format!("__hew_release_{action:?}")
}

fn scratch<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    frame: &coro::Frame<'ctx>,
    ty: BasicTypeEnum<'ctx>,
    name: &str,
) -> CodegenResult<PointerValue<'ctx>> {
    let builder = values.ctx.create_builder();
    if let Some(end) = frame.allocations.get_terminator() {
        builder.position_before(&end);
    } else {
        builder.position_at_end(frame.allocations);
    }
    builder
        .build_alloca(ty, name)
        .llvm_ctx("allocate release continuation storage")
}

pub(super) fn callback<'ctx>(
    ctx: &'ctx Context,
    llvm: &Module<'ctx>,
    module: &PhysicalModule,
    layout: &PhysicalLayout,
    action: DestroyAction,
) -> CodegenResult<FunctionValue<'ctx>> {
    let name = symbol(action);
    custom(ctx, llvm, module, &name, |values, frame, source| {
        if module.releases.suspends(action) {
            body(values, frame, source, layout, action)
        } else {
            let value = values
                .builder
                .build_load(llvm_type(ctx, &layout.repr)?, source, "release.sync.owner")
                .llvm_ctx("consume synchronous callback owner")?;
            values.destroy_loaded_value(value, layout, action)
        }
    })
}

pub(super) fn custom<'ctx>(
    ctx: &'ctx Context,
    llvm: &Module<'ctx>,
    module: &PhysicalModule,
    name: &str,
    emit: impl FnOnce(
        &ValueEmitter<'_, 'ctx>,
        &coro::Frame<'ctx>,
        PointerValue<'ctx>,
    ) -> CodegenResult<()>,
) -> CodegenResult<FunctionValue<'ctx>> {
    if let Some(function) = llvm.get_function(name) {
        return Ok(function);
    }
    let pointer = ctx.ptr_type(AddressSpace::default());
    let function = llvm.add_function(
        name,
        pointer.fn_type(&[pointer.into(); 3], false),
        Some(Linkage::Internal),
    );
    let builder = ctx.create_builder();
    builder.position_at_end(ctx.append_basic_block(function, "entry"));
    let source = function.get_nth_param(0).unwrap().into_pointer_value();
    let fault = function.get_nth_param(1).unwrap().into_pointer_value();
    let state = function.get_nth_param(2).unwrap().into_pointer_value();
    let frame = coro::begin(ctx, llvm, &builder, function, state)?;
    let status = builder
        .build_alloca(ctx.i32_type(), "release.status")
        .llvm_ctx("allocate release outcome")?;
    builder
        .build_store(status, ctx.i32_type().const_zero())
        .llvm_ctx("initialize release status")?;
    builder
        .build_store(fault, pointer.const_null())
        .llvm_ctx("initialize release fault")?;
    let values = ValueEmitter {
        module,
        ctx,
        llvm,
        builder: &builder,
        value: function,
        fault_sink: Some((fault, status)),
    };
    emit(&values, &frame, source)?;
    publish_fault(&values, &frame)?;
    let status = builder
        .build_load(ctx.i32_type(), status, "release.outcome")
        .llvm_ctx("read release outcome")?;
    let finish = coro::external(
        llvm,
        "hew_coro_state_finish",
        ctx.i32_type()
            .fn_type(&[pointer.into(), ctx.i32_type().into()], false),
    )?;
    builder
        .build_call(finish, &[state.into(), status.into()], "")
        .llvm_ctx("finish consuming release")?;
    builder
        .build_unconditional_branch(frame.finish)
        .llvm_ctx("complete release continuation")?;
    Ok(function)
}

fn invoke<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    frame: &coro::Frame<'ctx>,
    callee: PointerValue<'ctx>,
    source: PointerValue<'ctx>,
) -> CodegenResult<()> {
    publish_fault(values, frame)?;
    let pointer = values.ctx.ptr_type(AddressSpace::default());
    let fault = scratch(values, frame, pointer.into(), "release.child.fault")?;
    values
        .builder
        .build_store(fault, pointer.const_null())
        .llvm_ctx("clear release child fault")?;
    let create = coro::external(
        values.llvm,
        "hew_coro_state_cleanup_child",
        pointer.fn_type(&[pointer.into()], false),
    )?;
    let child = suspend::call_value(
        values.builder,
        create,
        &[frame.state.into()],
        "release.child.state",
    )?
    .into_pointer_value();
    let child_frame = values
        .builder
        .build_indirect_call(
            pointer.fn_type(&[pointer.into(); 3], false),
            callee,
            &[source.into(), fault.into(), child.into()],
            "release.child.frame",
        )
        .llvm_ctx("start consuming child release")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("release start returned no frame".into()))?
        .into_pointer_value();
    let status = suspend::await_child(
        values.ctx,
        values.llvm,
        values.builder,
        values.value,
        frame,
        child,
        child_frame,
    )?;
    combine(values, frame, fault, status)
}

fn publish_fault<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    frame: &coro::Frame<'ctx>,
) -> CodegenResult<()> {
    let (fault, _) = values
        .fault_sink
        .ok_or_else(|| CodegenError::FailClosed("release context lacks a fault slot".into()))?;
    let pointer = values.ctx.ptr_type(AddressSpace::default());
    let raised = values
        .builder
        .build_load(pointer, fault, "release.context.fault")
        .llvm_ctx("borrow retained cleanup fault")?;
    let publish = coro::external(
        values.llvm,
        "hew_coro_state_set_cleanup_fault",
        values.ctx.void_type().fn_type(&[pointer.into(); 2], false),
    )?;
    values
        .builder
        .build_call(publish, &[frame.state.into(), raised.into()], "")
        .llvm_ctx("publish cleanup context to following siblings")?;
    Ok(())
}

fn combine<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    frame: &coro::Frame<'ctx>,
    fault: PointerValue<'ctx>,
    status: IntValue<'ctx>,
) -> CodegenResult<()> {
    let failed = values
        .ctx
        .append_basic_block(values.value, "release.failed");
    let done = values
        .ctx
        .append_basic_block(values.value, "release.continue");
    let ok = values
        .builder
        .build_int_compare(
            IntPredicate::EQ,
            status,
            status.get_type().const_zero(),
            "release.ok",
        )
        .llvm_ctx("test release outcome")?;
    values
        .builder
        .build_conditional_branch(ok, done, failed)
        .llvm_ctx("preserve release failure")?;
    values.builder.position_at_end(failed);
    let raised = values
        .builder
        .build_load(
            values.ctx.ptr_type(AddressSpace::default()),
            fault,
            "release.fault",
        )
        .llvm_ctx("take release fault")?;
    let (primary, primary_status) = values
        .fault_sink
        .ok_or_else(|| CodegenError::FailClosed("consuming release has no fault owner".into()))?;
    values.record_release_fault(primary, primary_status, raised, status)?;
    publish_fault(values, frame)?;
    values
        .builder
        .build_unconditional_branch(done)
        .llvm_ctx("continue releasing siblings")?;
    values.builder.position_at_end(done);
    Ok(())
}

pub(super) fn slot<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    frame: &coro::Frame<'ctx>,
    source: PointerValue<'ctx>,
    layout: &PhysicalLayout,
    action: DestroyAction,
) -> CodegenResult<()> {
    if values.module.releases.suspends(action) {
        let callee = callback(values.ctx, values.llvm, values.module, layout, action)?;
        invoke(
            values,
            frame,
            callee.as_global_value().as_pointer_value(),
            source,
        )
    } else {
        let value = values
            .builder
            .build_load(
                llvm_type(values.ctx, &layout.repr)?,
                source,
                "release.value",
            )
            .llvm_ctx("load synchronous release value")?;
        values.destroy_loaded_value(value, layout, action)?;
        publish_fault(values, frame)
    }
}

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    pub(super) fn release_loaded(
        &self,
        value: BasicValueEnum<'ctx>,
        layout: &PhysicalLayout,
        action: DestroyAction,
    ) -> CodegenResult<()> {
        let values = self.value_emitter();
        if !self.module.releases.suspends(action) {
            return values.destroy_loaded_value(value, layout, action);
        }
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("resumable destruction lacks caller continuation".into())
        })?;
        let source = scratch(&values, frame, value.get_type(), "release.owner")?;
        self.builder
            .build_store(source, value)
            .llvm_ctx("transfer owner to consuming release")?;
        slot(&values, frame, source, layout, action)
    }
}

fn fields<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    frame: &coro::Frame<'ctx>,
    source: PointerValue<'ctx>,
    object: inkwell::types::StructType<'ctx>,
    recipes: &[PhysicalValueRecipe],
) -> CodegenResult<()> {
    for (index, recipe) in recipes.iter().enumerate().rev() {
        let Some(action) = recipe.destroy else {
            continue;
        };
        let field = values
            .builder
            .build_struct_gep(object, source, index as u32, "release.field")
            .llvm_ctx("address owned release field")?;
        let layout = values
            .module
            .target
            .layout(&recipe.ty)
            .ok_or_else(|| CodegenError::FailClosed("release field lacks layout".into()))?;
        slot(values, frame, field, layout, action)?;
    }
    Ok(())
}

fn body<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    frame: &coro::Frame<'ctx>,
    source: PointerValue<'ctx>,
    layout: &PhysicalLayout,
    action: DestroyAction,
) -> CodegenResult<()> {
    let pointer = values.ctx.ptr_type(AddressSpace::default());
    match action {
        DestroyAction::Resource(id) => match &values.module.resources[id.0 as usize].release {
            hew_mir::physical::ResourceRelease::RecordClose { close, .. }
            | hew_mir::physical::ResourceRelease::OpaqueClose { close, .. } => {
                let signature = callable(values.module, *close)?;
                let callee = values
                    .llvm
                    .get_function(&format!(
                        "{}$resume",
                        emitted_symbol(values.module, signature)
                    ))
                    .ok_or_else(|| {
                        CodegenError::FailClosed("resumable close lacks its selected ramp".into())
                    })?;
                let receiver = match signature.params[0].carrier {
                    ParamCarrier::Indirect => source.into(),
                    ParamCarrier::Direct => values
                        .builder
                        .build_load(
                            llvm_type(values.ctx, &layout.repr)?,
                            source,
                            "release.receiver",
                        )
                        .llvm_ctx("take close receiver")?
                        .into(),
                };
                let fault = scratch(values, frame, pointer.into(), "release.authored.fault")?;
                values
                    .builder
                    .build_store(fault, pointer.const_null())
                    .llvm_ctx("clear authored close fault")?;
                let status = suspend::invoke_child(
                    values.ctx,
                    values.llvm,
                    values.builder,
                    values.value,
                    frame,
                    callee,
                    &[receiver, fault.into()],
                )?;
                combine(values, frame, fault, status)
            }
            hew_mir::physical::ResourceRelease::Generator => generator(values, frame, source),
            hew_mir::physical::ResourceRelease::ActorCall => {
                let owner = values
                    .builder
                    .build_load(pointer, source, "release.call")
                    .llvm_ctx("consume actor call")?
                    .into_pointer_value();
                drain_operation(
                    values,
                    frame,
                    owner,
                    "hew_actor_call_cleanup_poll",
                    "hew_actor_call_cleanup_fault",
                )?;
                let free = external_drop(values.ctx, values.llvm, "hew_actor_call_free")?;
                values
                    .builder
                    .build_call(free, &[owner.into()], "")
                    .llvm_ctx("free drained actor call")?;
                Ok(())
            }
            hew_mir::physical::ResourceRelease::Stream => cursor(
                values,
                frame,
                source,
                "hew_stream_release_begin",
                false,
                None,
            ),
            hew_mir::physical::ResourceRelease::Sink => cursor(
                values,
                frame,
                source,
                "hew_sink_release_begin",
                false,
                Some(frame.state.into()),
            ),
            _ => Err(CodegenError::FailClosed(
                "synchronous resource selected for consuming continuation".into(),
            )),
        },
        DestroyAction::Aggregate(id) => fields(
            values,
            frame,
            source,
            llvm_type(values.ctx, &layout.repr)?.into_struct_type(),
            &values.aggregate_glue(id)?.fields,
        ),
        DestroyAction::Variant(id) => {
            let glue = values.variant_glue(id)?;
            let variant = values.variant_layout(&glue.ty)?;
            let node = values.variant_object_ptr(source, variant)?;
            let tag = values.load_variant_tag(node, variant)?;
            let invalid = values
                .ctx
                .append_basic_block(values.value, "release.variant.invalid");
            let done = values
                .ctx
                .append_basic_block(values.value, "release.variant.done");
            let cases = glue
                .variants
                .iter()
                .enumerate()
                .map(|(index, _)| {
                    (
                        tag.get_type().const_int(index as u64, false),
                        values
                            .ctx
                            .append_basic_block(values.value, "release.variant.case"),
                    )
                })
                .collect::<Vec<_>>();
            values
                .builder
                .build_switch(tag, invalid, &cases)
                .llvm_ctx("select consuming variant release")?;
            for (index, (_, block)) in cases.iter().enumerate() {
                values.builder.position_at_end(*block);
                if !glue.variants[index].fields.is_empty() {
                    let payload = values.variant_payload_ptr(node, variant)?;
                    fields(
                        values,
                        frame,
                        payload,
                        llvm_type(values.ctx, &variant.variants[index].repr)?.into_struct_type(),
                        &glue.variants[index].fields,
                    )?;
                }
                values
                    .builder
                    .build_unconditional_branch(done)
                    .llvm_ctx("complete variant fields")?;
            }
            values.builder.position_at_end(invalid);
            values.emit_invalid_variant_tag()?;
            values.builder.position_at_end(done);
            if variant.is_indirect {
                values.free_variant_node(node, variant)?;
            }
            Ok(())
        }
        DestroyAction::Vector(_) => {
            cursor(values, frame, source, "hew_vec_release_begin", false, None)
        }
        DestroyAction::Array(_) => cursor(
            values,
            frame,
            source,
            "hew_array_release_begin",
            false,
            None,
        ),
        DestroyAction::Map(_) => cursor(
            values,
            frame,
            source,
            "hew_hashmap_release_begin",
            false,
            None,
        ),
        DestroyAction::Set(_) => cursor(
            values,
            frame,
            source,
            "hew_hashset_release_begin",
            false,
            None,
        ),
        DestroyAction::Callable => cursor(
            values,
            frame,
            source,
            "hew_callable_release_begin",
            true,
            None,
        ),
        DestroyAction::RcRelease(id) => {
            let name = format!("__hew_shared_payload_{}_layout", id.0);
            let target = TargetData::create(&values.module.target.data_layout);
            let descriptor = values.llvm.get_global(&name).unwrap_or_else(|| {
                values
                    .llvm
                    .add_global(value_descriptor_type(values.ctx, &target), None, &name)
            });
            cursor(
                values,
                frame,
                source,
                "hew_rc_release_begin",
                false,
                Some(descriptor.as_pointer_value().into()),
            )
        }
        DestroyAction::TraitObject => cursor(
            values,
            frame,
            source,
            "hew_trait_object_release_begin",
            true,
            Some(values.ctx.i32_type().const_int(1, false).into()),
        ),
        _ => Err(CodegenError::FailClosed(
            "plain release selected for continuation".into(),
        )),
    }
}

pub(super) fn drain_operation<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    frame: &coro::Frame<'ctx>,
    owner: PointerValue<'ctx>,
    poll_name: &str,
    fault_name: &str,
) -> CodegenResult<()> {
    let pointer = values.ctx.ptr_type(AddressSpace::default());
    let poll = values
        .ctx
        .append_basic_block(values.value, "release.operation.poll");
    let wait = values
        .ctx
        .append_basic_block(values.value, "release.operation.wait");
    let done = values
        .ctx
        .append_basic_block(values.value, "release.operation.done");
    let invalid = values
        .ctx
        .append_basic_block(values.value, "release.operation.invalid");
    values
        .builder
        .build_unconditional_branch(poll)
        .llvm_ctx("drain operation owners")?;
    values.builder.position_at_end(poll);
    let poll_fn = coro::external(
        values.llvm,
        poll_name,
        values.ctx.i32_type().fn_type(&[pointer.into(); 2], false),
    )?;
    let status = suspend::call_value(
        values.builder,
        poll_fn,
        &[owner.into(), frame.state.into()],
        "release.operation.status",
    )?
    .into_int_value();
    values
        .builder
        .build_switch(status, done, &[(values.ctx.i32_type().const_zero(), wait)])
        .llvm_ctx("wait for operation cleanup")?;
    values.builder.position_at_end(wait);
    frame.suspend(
        values.ctx,
        values.llvm,
        values.builder,
        poll,
        invalid,
        false,
    )?;
    values.builder.position_at_end(invalid);
    values
        .builder
        .build_unreachable()
        .llvm_ctx("refuse unfinished operation destruction")?;
    values.builder.position_at_end(done);
    let take_fault = coro::external(
        values.llvm,
        fault_name,
        pointer.fn_type(&[pointer.into()], false),
    )?;
    let raised = suspend::call_value(
        values.builder,
        take_fault,
        &[owner.into()],
        "release.operation.fault",
    )?;
    let fault = scratch(
        values,
        frame,
        pointer.into(),
        "release.operation.fault.slot",
    )?;
    values
        .builder
        .build_store(fault, raised)
        .llvm_ctx("own operation cleanup fault")?;
    let code_fn = coro::external(
        values.llvm,
        "hew_fault_code",
        values.ctx.i32_type().fn_type(&[pointer.into()], false),
    )?;
    let code = suspend::call_value(
        values.builder,
        code_fn,
        &[raised.into()],
        "release.operation.code",
    )?
    .into_int_value();
    combine(values, frame, fault, code)
}

fn cursor<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    frame: &coro::Frame<'ctx>,
    source: PointerValue<'ctx>,
    begin_name: &str,
    by_slot: bool,
    extra: Option<BasicMetadataValueEnum<'ctx>>,
) -> CodegenResult<()> {
    let pointer = values.ctx.ptr_type(AddressSpace::default());
    let owner = if by_slot {
        source.into()
    } else {
        values
            .builder
            .build_load(pointer, source, "release.container")
            .llvm_ctx("consume container owner")?
            .into()
    };
    let mut types = vec![pointer.into()];
    let mut args = vec![owner];
    if let Some(extra) = extra {
        types.push(match extra {
            BasicMetadataValueEnum::IntValue(value) => value.get_type().into(),
            _ => pointer.into(),
        });
        args.push(extra);
    }
    let begin = coro::external(values.llvm, begin_name, pointer.fn_type(&types, false))?;
    let cursor =
        suspend::call_value(values.builder, begin, &args, "release.cursor")?.into_pointer_value();
    drain_cursor(values, frame, cursor)
}

pub(super) fn drain_cursor<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    frame: &coro::Frame<'ctx>,
    cursor: PointerValue<'ctx>,
) -> CodegenResult<()> {
    let pointer = values.ctx.ptr_type(AddressSpace::default());
    let next = values
        .ctx
        .append_basic_block(values.value, "release.cursor.next");
    let item = values
        .ctx
        .append_basic_block(values.value, "release.cursor.item");
    let done = values
        .ctx
        .append_basic_block(values.value, "release.cursor.done");
    let present = values
        .builder
        .build_is_not_null(cursor, "release.cursor.present")
        .llvm_ctx("test optional release cursor")?;
    values
        .builder
        .build_conditional_branch(present, next, done)
        .llvm_ctx("start consuming container")?;
    values.builder.position_at_end(next);
    let advance = coro::external(
        values.llvm,
        "hew_release_next",
        pointer.fn_type(&[pointer.into()], false),
    )?;
    let source = suspend::call_value(values.builder, advance, &[cursor.into()], "release.slot")?
        .into_pointer_value();
    let present = values
        .builder
        .build_is_not_null(source, "release.item.present")
        .llvm_ctx("test next release item")?;
    values
        .builder
        .build_conditional_branch(present, item, done)
        .llvm_ctx("select next release item")?;
    values.builder.position_at_end(item);
    let get_layout = coro::external(
        values.llvm,
        "hew_release_layout",
        pointer.fn_type(&[pointer.into()], false),
    )?;
    let descriptor = suspend::call_value(
        values.builder,
        get_layout,
        &[cursor.into()],
        "release.layout",
    )?
    .into_pointer_value();
    descriptor_slot(values, frame, source, descriptor)?;
    values
        .builder
        .build_unconditional_branch(next)
        .llvm_ctx("continue consuming container")?;
    values.builder.position_at_end(done);
    let finish = coro::external(
        values.llvm,
        "hew_release_finish",
        values.ctx.void_type().fn_type(&[pointer.into()], false),
    )?;
    values
        .builder
        .build_call(finish, &[cursor.into()], "")
        .llvm_ctx("release drained container storage")?;
    Ok(())
}

fn descriptor_slot<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    frame: &coro::Frame<'ctx>,
    source: PointerValue<'ctx>,
    descriptor: PointerValue<'ctx>,
) -> CodegenResult<()> {
    let pointer = values.ctx.ptr_type(AddressSpace::default());
    let target = TargetData::create(&values.module.target.data_layout);
    let layout = value_descriptor_type(values.ctx, &target);
    let address = values
        .builder
        .build_struct_gep(layout, descriptor, 6, "release.start.slot")
        .llvm_ctx("address descriptor continuation")?;
    let start = values
        .builder
        .build_load(pointer, address, "release.start")
        .llvm_ctx("read descriptor continuation")?
        .into_pointer_value();
    let asynchronous = values.ctx.append_basic_block(values.value, "release.async");
    let synchronous = values.ctx.append_basic_block(values.value, "release.sync");
    let done = values
        .ctx
        .append_basic_block(values.value, "release.item.done");
    let has_start = values
        .builder
        .build_is_not_null(start, "release.can.suspend")
        .llvm_ctx("select release protocol")?;
    values
        .builder
        .build_conditional_branch(has_start, asynchronous, synchronous)
        .llvm_ctx("dispatch release protocol")?;
    values.builder.position_at_end(asynchronous);
    invoke(values, frame, start, source)?;
    values
        .builder
        .build_unconditional_branch(done)
        .llvm_ctx("finish asynchronous release")?;
    values.builder.position_at_end(synchronous);
    let drop_address = values
        .builder
        .build_struct_gep(layout, descriptor, 4, "release.drop.slot")
        .llvm_ctx("address synchronous destructor")?;
    let drop = values
        .builder
        .build_load(pointer, drop_address, "release.drop")
        .llvm_ctx("read synchronous destructor")?
        .into_pointer_value();
    let run = values
        .ctx
        .append_basic_block(values.value, "release.sync.run");
    let has_drop = values
        .builder
        .build_is_not_null(drop, "release.owned")
        .llvm_ctx("test synchronous owner")?;
    values
        .builder
        .build_conditional_branch(has_drop, run, done)
        .llvm_ctx("select synchronous owner release")?;
    values.builder.position_at_end(run);
    values.emit_release_in_sink(|| {
        values
            .builder
            .build_indirect_call(
                values.ctx.void_type().fn_type(&[pointer.into()], false),
                drop,
                &[source.into()],
                "",
            )
            .llvm_ctx("release synchronous descriptor value")?;
        Ok(())
    })?;
    values
        .builder
        .build_unconditional_branch(done)
        .llvm_ctx("finish synchronous release")?;
    values.builder.position_at_end(done);
    Ok(())
}

fn generator<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    frame: &coro::Frame<'ctx>,
    source: PointerValue<'ctx>,
) -> CodegenResult<()> {
    let pointer = values.ctx.ptr_type(AddressSpace::default());
    let owner = values
        .builder
        .build_load(pointer, source, "release.generator")
        .llvm_ctx("consume generator")?;
    let fault = scratch(values, frame, pointer.into(), "release.generator.fault")?;
    let poll = values
        .ctx
        .append_basic_block(values.value, "release.generator.poll");
    let wait = values
        .ctx
        .append_basic_block(values.value, "release.generator.wait");
    let done = values
        .ctx
        .append_basic_block(values.value, "release.generator.done");
    let invalid = values
        .ctx
        .append_basic_block(values.value, "release.generator.invalid");
    values
        .builder
        .build_unconditional_branch(poll)
        .llvm_ctx("drain generator before release")?;
    values.builder.position_at_end(poll);
    values
        .builder
        .build_store(fault, pointer.const_null())
        .llvm_ctx("clear generator cleanup fault")?;
    let poll_fn = coro::external(
        values.llvm,
        "hew_checked_generator_close_poll",
        values.ctx.i32_type().fn_type(&[pointer.into(); 3], false),
    )?;
    let status = suspend::call_value(
        values.builder,
        poll_fn,
        &[owner.into(), frame.state.into(), fault.into()],
        "release.generator.status",
    )?
    .into_int_value();
    values
        .builder
        .build_switch(status, done, &[(values.ctx.i32_type().const_zero(), wait)])
        .llvm_ctx("wait for generator cleanup")?;
    values.builder.position_at_end(wait);
    frame.suspend(
        values.ctx,
        values.llvm,
        values.builder,
        poll,
        invalid,
        false,
    )?;
    values.builder.position_at_end(invalid);
    values
        .builder
        .build_unreachable()
        .llvm_ctx("refuse unfinished generator destruction")?;
    values.builder.position_at_end(done);
    let raised = values
        .builder
        .build_load(pointer, fault, "release.generator.raised")
        .llvm_ctx("read generator cleanup fault")?
        .into_pointer_value();
    let fault_code = coro::external(
        values.llvm,
        "hew_fault_code",
        values.ctx.i32_type().fn_type(&[pointer.into()], false),
    )?;
    let code = suspend::call_value(
        values.builder,
        fault_code,
        &[raised.into()],
        "release.generator.code",
    )?
    .into_int_value();
    combine(values, frame, fault, code)?;
    let free = external_drop(values.ctx, values.llvm, "hew_checked_generator_free")?;
    values
        .builder
        .build_call(free, &[owner.into()], "")
        .llvm_ctx("free completed generator")?;
    Ok(())
}
