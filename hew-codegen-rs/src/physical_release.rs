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
    let finish = get_or_declare_external(
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
    let create = get_or_declare_external(
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
    combine(values, frame.state, fault, status)
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
    let publish = get_or_declare_external(
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

/// Fold a finished release into the frame's fault record: a non-zero `status`
/// joins the raised fault in `fault` to the sink and republishes the sink to
/// the siblings still to release. The rule is emitted once as
/// `__hew_release_outcome`, so a release site is one call.
fn combine<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    state: PointerValue<'ctx>,
    fault: PointerValue<'ctx>,
    status: IntValue<'ctx>,
) -> CodegenResult<()> {
    let (primary, primary_status) = values
        .fault_sink
        .ok_or_else(|| CodegenError::FailClosed("consuming release has no fault owner".into()))?;
    let pointer = values.ctx.ptr_type(AddressSpace::default());
    let name = "__hew_release_outcome";
    let thunk = if let Some(thunk) = values.llvm.get_function(name) {
        thunk
    } else {
        let thunk = values.llvm.add_function(
            name,
            values.ctx.void_type().fn_type(
                &[
                    pointer.into(),
                    pointer.into(),
                    pointer.into(),
                    values.ctx.i32_type().into(),
                    pointer.into(),
                ],
                false,
            ),
            Some(Linkage::Internal),
        );
        thunk.add_attribute(
            inkwell::attributes::AttributeLoc::Function,
            values.ctx.create_enum_attribute(
                inkwell::attributes::Attribute::get_named_enum_kind_id("noinline"),
                0,
            ),
        );
        let builder = values.ctx.create_builder();
        builder.position_at_end(values.ctx.append_basic_block(thunk, "entry"));
        let inner = ValueEmitter {
            module: values.module,
            ctx: values.ctx,
            llvm: values.llvm,
            builder: &builder,
            value: thunk,
            fault_sink: None,
        };
        let param = |index| thunk.get_nth_param(index).unwrap();
        let failed = values.ctx.append_basic_block(thunk, "release.failed");
        let done = values.ctx.append_basic_block(thunk, "release.continue");
        let ok = builder
            .build_int_compare(
                IntPredicate::EQ,
                param(3).into_int_value(),
                values.ctx.i32_type().const_zero(),
                "release.ok",
            )
            .llvm_ctx("test release outcome")?;
        builder
            .build_conditional_branch(ok, done, failed)
            .llvm_ctx("preserve release failure")?;
        builder.position_at_end(failed);
        let raised = builder
            .build_load(pointer, param(2).into_pointer_value(), "release.fault")
            .llvm_ctx("take release fault")?;
        inner.fold_release_fault(
            param(0).into_pointer_value(),
            param(1).into_pointer_value(),
            raised,
            param(3).into_int_value(),
        )?;
        let active = builder
            .build_load(
                pointer,
                param(0).into_pointer_value(),
                "release.context.fault",
            )
            .llvm_ctx("borrow retained cleanup fault")?;
        let publish = get_or_declare_external(
            values.llvm,
            "hew_coro_state_set_cleanup_fault",
            values.ctx.void_type().fn_type(&[pointer.into(); 2], false),
        )?;
        builder
            .build_call(publish, &[param(4).into(), active.into()], "")
            .llvm_ctx("publish cleanup context to following siblings")?;
        builder
            .build_unconditional_branch(done)
            .llvm_ctx("continue releasing siblings")?;
        builder.position_at_end(done);
        builder
            .build_return(None)
            .llvm_ctx("finish release outcome")?;
        thunk
    };
    values
        .builder
        .build_call(
            thunk,
            &[
                primary.into(),
                primary_status.into(),
                fault.into(),
                status.into(),
                state.into(),
            ],
            "",
        )
        .llvm_ctx("fold the release outcome")?;
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

/// A runtime operation handle whose release drains the operation before it
/// frees it. Every site releases through one `__hew_release_handle_*` thunk per
/// kind, so a suspending site carries one call rather than the drain loop.
#[derive(Clone, Copy)]
pub(super) enum Handle {
    ActorCall,
    RemoteCall,
    /// A detached-owner cursor, or null when nothing was displaced.
    Cursor,
}

impl Handle {
    const fn thunk(self) -> &'static str {
        match self {
            Self::ActorCall => "__hew_release_handle_actor_call",
            Self::RemoteCall => "__hew_release_handle_remote_call",
            Self::Cursor => "__hew_release_handle_cursor",
        }
    }

    /// The runtime's cleanup poll, cleanup fault and free for a drained
    /// operation; a cursor has none.
    const fn operation(self) -> Option<(&'static str, &'static str, &'static str)> {
        match self {
            Self::ActorCall => Some((
                "hew_actor_call_cleanup_poll",
                "hew_actor_call_cleanup_fault",
                "hew_actor_call_free",
            )),
            Self::RemoteCall => Some((
                "hew_remote_call_cleanup_poll",
                "hew_remote_call_cleanup_fault",
                "hew_remote_call_free",
            )),
            Self::Cursor => None,
        }
    }
}

/// Drain and free `handle` through its shared thunk; a drain fault joins the
/// caller's active fault after every remaining owner is released.
pub(super) fn handle<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    frame: &coro::Frame<'ctx>,
    handle: PointerValue<'ctx>,
    kind: Handle,
) -> CodegenResult<()> {
    // An operation with nothing left to drain, the usual case, completes in
    // one plain call; only an unfinished drain starts the suspending thunk.
    let settled = if let Some(operation) = kind.operation() {
        let ready = ready_release(values, kind, operation)?;
        let (fault, status) = values
            .fault_sink
            .ok_or_else(|| CodegenError::FailClosed("release context lacks a fault slot".into()))?;
        let done = suspend::call_value(
            values.builder,
            ready,
            &[
                handle.into(),
                frame.state.into(),
                fault.into(),
                status.into(),
            ],
            "release.operation.ready",
        )?
        .into_int_value();
        let draining = values
            .ctx
            .append_basic_block(values.value, "release.operation.drain");
        let settled = values
            .ctx
            .append_basic_block(values.value, "release.operation.settled");
        let finished = values
            .builder
            .build_int_compare(
                IntPredicate::NE,
                done,
                done.get_type().const_zero(),
                "release.operation.finished",
            )
            .llvm_ctx("test settled operation")?;
        values
            .builder
            .build_conditional_branch(finished, settled, draining)
            .llvm_ctx("skip the drain of a settled operation")?;
        values.builder.position_at_end(draining);
        Some(settled)
    } else {
        None
    };
    let callee = custom(
        values.ctx,
        values.llvm,
        values.module,
        kind.thunk(),
        |values, frame, source| handle_body(values, frame, source, kind),
    )?;
    let pointer = values.ctx.ptr_type(AddressSpace::default());
    let source = scratch(values, frame, pointer.into(), "release.handle")?;
    values
        .builder
        .build_store(source, handle)
        .llvm_ctx("transfer operation handle to consuming release")?;
    invoke(
        values,
        frame,
        callee.as_global_value().as_pointer_value(),
        source,
    )?;
    if let Some(settled) = settled {
        values
            .builder
            .build_unconditional_branch(settled)
            .llvm_ctx("finish the drained operation")?;
        values.builder.position_at_end(settled);
    }
    Ok(())
}

/// The plain function that releases an operation whose cleanup is already
/// complete: poll once, and when ready fold any cleanup fault into the
/// caller's record and free it, returning one. A zero return leaves the
/// operation untouched for the suspending thunk to drain.
fn ready_release<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    kind: Handle,
    (poll, fault, free): (&str, &str, &str),
) -> CodegenResult<FunctionValue<'ctx>> {
    let name = format!("{}$ready", kind.thunk());
    if let Some(function) = values.llvm.get_function(&name) {
        return Ok(function);
    }
    let pointer = values.ctx.ptr_type(AddressSpace::default());
    let i32_ty = values.ctx.i32_type();
    let function = values.llvm.add_function(
        &name,
        i32_ty.fn_type(&[pointer.into(); 4], false),
        Some(Linkage::Internal),
    );
    function.add_attribute(
        inkwell::attributes::AttributeLoc::Function,
        values.ctx.create_enum_attribute(
            inkwell::attributes::Attribute::get_named_enum_kind_id("noinline"),
            0,
        ),
    );
    let builder = values.ctx.create_builder();
    builder.position_at_end(values.ctx.append_basic_block(function, "entry"));
    let param = |index| function.get_nth_param(index).unwrap().into_pointer_value();
    let (owner, state) = (param(0), param(1));
    let inner = ValueEmitter {
        module: values.module,
        ctx: values.ctx,
        llvm: values.llvm,
        builder: &builder,
        value: function,
        fault_sink: Some((param(2), param(3))),
    };
    let settled = values.ctx.append_basic_block(function, "settled");
    let pending = values.ctx.append_basic_block(function, "pending");
    let poll_fn = get_or_declare_external(
        values.llvm,
        poll,
        i32_ty.fn_type(&[pointer.into(); 2], false),
    )?;
    let status = suspend::call_value(
        &builder,
        poll_fn,
        &[owner.into(), state.into()],
        "release.operation.status",
    )?
    .into_int_value();
    let waiting = builder
        .build_int_compare(
            IntPredicate::EQ,
            status,
            i32_ty.const_zero(),
            "release.operation.waiting",
        )
        .llvm_ctx("test unfinished operation cleanup")?;
    builder
        .build_conditional_branch(waiting, pending, settled)
        .llvm_ctx("wait for operation cleanup")?;
    builder.position_at_end(pending);
    builder
        .build_return(Some(&i32_ty.const_zero()))
        .llvm_ctx("leave an unfinished drain to the thunk")?;
    builder.position_at_end(settled);
    let take_fault = get_or_declare_external(
        values.llvm,
        fault,
        pointer.fn_type(&[pointer.into()], false),
    )?;
    let raised = suspend::call_value(
        &builder,
        take_fault,
        &[owner.into()],
        "release.operation.fault",
    )?;
    let slot = builder
        .build_alloca(pointer, "release.operation.fault.slot")
        .llvm_ctx("own operation cleanup fault")?;
    builder
        .build_store(slot, raised)
        .llvm_ctx("own operation cleanup fault")?;
    let code_fn = get_or_declare_external(
        values.llvm,
        "hew_fault_code",
        i32_ty.fn_type(&[pointer.into()], false),
    )?;
    let code = suspend::call_value(
        &builder,
        code_fn,
        &[raised.into()],
        "release.operation.code",
    )?
    .into_int_value();
    combine(&inner, state, slot, code)?;
    let free = external_drop(values.ctx, values.llvm, free)?;
    builder
        .build_call(free, &[owner.into()], "")
        .llvm_ctx("free drained operation")?;
    builder
        .build_return(Some(&i32_ty.const_int(1, false)))
        .llvm_ctx("finish a settled operation")?;
    Ok(function)
}

fn handle_body<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    frame: &coro::Frame<'ctx>,
    source: PointerValue<'ctx>,
    kind: Handle,
) -> CodegenResult<()> {
    let pointer = values.ctx.ptr_type(AddressSpace::default());
    let owner = values
        .builder
        .build_load(pointer, source, "release.handle.owner")
        .llvm_ctx("consume operation handle")?
        .into_pointer_value();
    let (poll, fault, free) = match kind {
        Handle::ActorCall => (
            "hew_actor_call_cleanup_poll",
            "hew_actor_call_cleanup_fault",
            "hew_actor_call_free",
        ),
        Handle::RemoteCall => (
            "hew_remote_call_cleanup_poll",
            "hew_remote_call_cleanup_fault",
            "hew_remote_call_free",
        ),
        Handle::Cursor => return drain_cursor_inline(values, frame, owner),
    };
    drain_operation(values, frame, owner, poll, fault)?;
    let free = external_drop(values.ctx, values.llvm, free)?;
    values
        .builder
        .build_call(free, &[owner.into()], "")
        .llvm_ctx("free drained operation")?;
    Ok(())
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

    /// Release a stream operation: any displaced owners drain through the
    /// shared cursor thunk, and nothing starts when none were displaced.
    pub(super) fn release_stream_operation(
        &self,
        operation: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("stream operation release lacks continuation".into())
        })?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let begin = get_or_declare_external(
            self.llvm,
            "hew_stream_operation_release_begin",
            pointer.fn_type(&[pointer.into()], false),
        )?;
        let cursor = suspend::call_value(
            &self.builder,
            begin,
            &[operation.into()],
            "stream.release.cursor",
        )?
        .into_pointer_value();
        drain_cursor(&self.value_emitter(), frame, cursor)
    }

    pub(super) fn release_handle(
        &self,
        kind: Handle,
        operation: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("operation release lacks caller continuation".into())
        })?;
        handle(&self.value_emitter(), frame, operation, kind)
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
                combine(values, frame.state, fault, status)
            }
            hew_mir::physical::ResourceRelease::Generator => generator(values, frame, source),
            hew_mir::physical::ResourceRelease::ActorRequest => cursor(
                values,
                frame,
                source,
                "hew_msg_envelope_release_begin",
                false,
                None,
            ),
            hew_mir::physical::ResourceRelease::ActorCall => {
                handle_body(values, frame, source, Handle::ActorCall)
            }
            hew_mir::physical::ResourceRelease::Stream => cursor(
                values,
                frame,
                source,
                "hew_stream_release_begin",
                false,
                None,
            ),
            hew_mir::physical::ResourceRelease::Sink => release_sink(values, frame, source),
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

fn release_sink<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    frame: &coro::Frame<'ctx>,
    source: PointerValue<'ctx>,
) -> CodegenResult<()> {
    let pointer = values.ctx.ptr_type(AddressSpace::default());
    let owner = values
        .builder
        .build_load(pointer, source, "sink.release.owner")
        .llvm_ctx("take sink owner for release")?
        .into_pointer_value();
    let begin = get_or_declare_external(
        values.llvm,
        "hew_sink_release_begin",
        pointer.fn_type(&[pointer.into(); 2], false),
    )?;
    let cursor = suspend::call_value(
        values.builder,
        begin,
        &[owner.into(), frame.state.into()],
        "sink.release.cursor",
    )?
    .into_pointer_value();
    let waker = get_or_declare_external(
        values.llvm,
        "hew_coro_state_waker",
        pointer.fn_type(&[pointer.into()], false),
    )?;
    let waker = suspend::call_value(
        values.builder,
        waker,
        &[frame.state.into()],
        "sink.release.waker",
    )?;
    let finish = get_or_declare_external(
        values.llvm,
        "hew_async_sink_finish",
        pointer.fn_type(&[pointer.into(); 2], false),
    )?;
    let request = suspend::call_value(
        values.builder,
        finish,
        &[owner.into(), waker.into()],
        "sink.release.finish",
    )?
    .into_pointer_value();
    drain_operation(
        values,
        frame,
        request,
        "hew_sink_finish_cleanup_poll",
        "hew_sink_finish_cleanup_fault",
    )?;
    let free = external_drop(values.ctx, values.llvm, "hew_async_io_free")?;
    values
        .builder
        .build_call(free, &[request.into()], "")
        .llvm_ctx("free sink finish request")?;
    drain_cursor_inline(values, frame, cursor)
}

fn drain_operation<'ctx>(
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
    let poll_fn = get_or_declare_external(
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
    let take_fault = get_or_declare_external(
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
    let code_fn = get_or_declare_external(
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
    combine(values, frame.state, fault, code)
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
    let begin = get_or_declare_external(values.llvm, begin_name, pointer.fn_type(&types, false))?;
    let cursor =
        suspend::call_value(values.builder, begin, &args, "release.cursor")?.into_pointer_value();
    drain_cursor_inline(values, frame, cursor)
}

/// Drain detached owners using the caller's checked suspension capability.
pub(super) fn drain<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    frame: Option<&coro::Frame<'ctx>>,
    cursor: PointerValue<'ctx>,
) -> CodegenResult<()> {
    if let Some(frame) = frame {
        return drain_cursor(values, frame, cursor);
    }
    let run = external_drop(values.ctx, values.llvm, "hew_release_sync")?;
    values.emit_release_in_sink(|| {
        values
            .builder
            .build_call(run, &[cursor.into()], "")
            .llvm_ctx("release synchronous displaced owners")?;
        Ok(())
    })
}

/// Drain detached owners through the shared cursor thunk. Nothing is started
/// when no owner was displaced.
pub(super) fn drain_cursor<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    frame: &coro::Frame<'ctx>,
    cursor: PointerValue<'ctx>,
) -> CodegenResult<()> {
    let drain = values
        .ctx
        .append_basic_block(values.value, "release.cursor.drain");
    let done = values
        .ctx
        .append_basic_block(values.value, "release.cursor.skip");
    let present = values
        .builder
        .build_is_not_null(cursor, "release.cursor.displaced")
        .llvm_ctx("test displaced owners")?;
    values
        .builder
        .build_conditional_branch(present, drain, done)
        .llvm_ctx("skip an empty cursor")?;
    values.builder.position_at_end(drain);
    handle(values, frame, cursor, Handle::Cursor)?;
    values
        .builder
        .build_unconditional_branch(done)
        .llvm_ctx("finish displaced owners")?;
    values.builder.position_at_end(done);
    Ok(())
}

fn drain_cursor_inline<'ctx>(
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
    let advance = get_or_declare_external(
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
    let get_layout = get_or_declare_external(
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
    let finish = get_or_declare_external(
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
        .build_struct_gep(layout, descriptor, 5, "release.start.slot")
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
    let poll_fn = get_or_declare_external(
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
    let fault_code = get_or_declare_external(
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
    combine(values, frame.state, fault, code)?;
    let free = external_drop(values.ctx, values.llvm, "hew_checked_generator_free")?;
    values
        .builder
        .build_call(free, &[owner.into()], "")
        .llvm_ctx("free completed generator")?;
    Ok(())
}
