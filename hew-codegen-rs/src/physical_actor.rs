//! Native actor adapters over verified state, message and callable contracts.

use super::suspend::call_value;
use super::*;
use hew_mir::physical::{ActorId, ActorOperation, SemActor, SemActorHandler};
use inkwell::types::StructType;

#[path = "physical_actor_ask.rs"]
mod ask;

fn symbol(actor: ActorId, suffix: &str) -> String {
    format!("__hew_actor_{}_{}", actor.0, suffix)
}

fn message_symbol(actor: ActorId, message: u32) -> String {
    symbol(actor, &format!("message_{message}_drop"))
}

fn reply_symbol(actor: ActorId, message: u32) -> String {
    symbol(actor, &format!("reply_{message}_drop"))
}

impl<'ctx> ModuleEmitter<'ctx, '_> {
    fn emit_actor_reply(
        &self,
        builder: &Builder<'ctx>,
        function: FunctionValue<'ctx>,
        actor: &SemActor,
        handler: &SemActorHandler,
        output: Option<PointerValue<'ctx>>,
        fault: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        if handler.return_ty == ResolvedTy::Unit {
            return Ok(());
        }
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let target = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let complete = self.ctx.append_basic_block(function, "reply.complete");
        let publish = self.ctx.append_basic_block(function, "reply.publish");
        let fault = builder
            .build_load(ptr, fault, "reply.fault")
            .llvm_ctx("inspect handler completion")?
            .into_pointer_value();
        let success = builder
            .build_is_null(fault, "reply.success")
            .llvm_ctx("check successful reply")?;
        builder
            .build_conditional_branch(success, publish, complete)
            .llvm_ctx("publish only an initialized reply")?;
        builder.position_at_end(publish);
        let size = if output.is_some() {
            self.module
                .target
                .layout(&handler.return_ty)
                .ok_or_else(|| CodegenError::FailClosed("reply has no target layout".into()))?
                .size
        } else {
            0
        };
        let drop_reply = self
            .llvm
            .get_function(&reply_symbol(actor.id, handler.message_id))
            .map_or(ptr.const_null(), |function| {
                function.as_global_value().as_pointer_value()
            });
        let reply = coro::external(
            &self.llvm,
            "hew_actor_reply_native",
            self.ctx
                .void_type()
                .fn_type(&[ptr.into(), size_ty.into(), ptr.into()], false),
        )?;
        builder
            .build_call(
                reply,
                &[
                    output.unwrap_or(ptr.const_null()).into(),
                    size_ty.const_int(size, false).into(),
                    drop_reply.into(),
                ],
                "",
            )
            .llvm_ctx("transfer typed reply under current activation")?;
        builder
            .build_unconditional_branch(complete)
            .llvm_ctx("complete typed reply")?;
        builder.position_at_end(complete);
        Ok(())
    }
}

fn message_type<'ctx>(
    module: &PhysicalModule,
    ctx: &'ctx Context,
    handler: &SemActorHandler,
) -> CodegenResult<StructType<'ctx>> {
    let mut fields = vec![ctx.i8_type().into()];
    for ty in &handler.params {
        fields.push(llvm_type(
            ctx,
            &module
                .target
                .layout(ty)
                .ok_or_else(|| {
                    CodegenError::FailClosed("actor message field lacks its target layout".into())
                })?
                .repr,
        )?);
    }
    Ok(ctx.struct_type(&fields, false))
}

fn allocate<'ctx>(
    module: &PhysicalModule,
    ctx: &'ctx Context,
    llvm: &Module<'ctx>,
    builder: &Builder<'ctx>,
    size: u64,
) -> CodegenResult<PointerValue<'ctx>> {
    let target = TargetData::create(&module.target.data_layout);
    let size_ty = ctx.ptr_sized_int_type(&target, None);
    let allocate = get_or_declare_external(
        llvm,
        "hew_actor_payload_alloc",
        ctx.ptr_type(AddressSpace::default())
            .fn_type(&[size_ty.into()], false),
    )?;
    builder
        .build_call(
            allocate,
            &[size_ty.const_int(size, false).into()],
            "actor.allocate",
        )
        .llvm_ctx("allocate actor wrapper")?
        .try_as_basic_value()
        .basic()
        .map(BasicValueEnum::into_pointer_value)
        .ok_or_else(|| CodegenError::FailClosed("actor allocation returned void".into()))
}

impl<'ctx> ModuleEmitter<'ctx, '_> {
    pub(super) fn emit_actor_runtime_start(
        &self,
        builder: &Builder<'ctx>,
        wrapper: FunctionValue<'ctx>,
    ) -> CodegenResult<()> {
        if self.module.actors.is_empty() {
            return Ok(());
        }
        let start = get_or_declare_external(
            &self.llvm,
            "hew_sched_init",
            self.ctx.i32_type().fn_type(&[], false),
        )?;
        let status = builder
            .build_call(start, &[], "runtime.start")
            .llvm_ctx("start required actor runtime")?
            .try_as_basic_value()
            .basic()
            .unwrap()
            .into_int_value();
        let ready = self.ctx.append_basic_block(wrapper, "runtime.ready");
        let failed = self.ctx.append_basic_block(wrapper, "runtime.failed");
        let ok = builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_zero(),
                "runtime.ok",
            )
            .llvm_ctx("check required runtime startup")?;
        builder
            .build_conditional_branch(ok, ready, failed)
            .llvm_ctx("enter source root after runtime startup")?;
        builder.position_at_end(failed);
        builder
            .build_return(Some(&status))
            .llvm_ctx("return runtime startup failure")?;
        builder.position_at_end(ready);
        Ok(())
    }

    pub(super) fn emit_actor_runtime_finish(
        &self,
        builder: &Builder<'ctx>,
        status: IntValue<'ctx>,
    ) -> CodegenResult<IntValue<'ctx>> {
        if self.module.actors.is_empty() {
            return Ok(status);
        }
        let finish = get_or_declare_external(
            &self.llvm,
            "hew_native_runtime_finish",
            self.ctx
                .i32_type()
                .fn_type(&[self.ctx.i32_type().into()], false),
        )?;
        builder
            .build_call(finish, &[status.into()], "runtime.finish")
            .llvm_ctx("drain actor work and finish process runtime")?
            .try_as_basic_value()
            .basic()
            .map(BasicValueEnum::into_int_value)
            .ok_or_else(|| CodegenError::FailClosed("runtime finish returned void".into()))
    }

    pub(super) fn emit_actor_descriptors(&self) -> CodegenResult<()> {
        for actor in &self.module.actors {
            self.emit_actor_state_callbacks(actor)?;
            for handler in &actor.handlers {
                self.emit_actor_message_drop(actor, handler)?;
                if let Some(recipe) = self.module.actor_recipes.get(&handler.return_ty) {
                    if let Some(action) = recipe.destroy {
                        let layout =
                            self.module
                                .target
                                .layout(&handler.return_ty)
                                .ok_or_else(|| {
                                    CodegenError::FailClosed("reply lacks its exact layout".into())
                                })?;
                        self.emit_value_drop_callback(
                            &reply_symbol(actor.id, handler.message_id),
                            layout,
                            action,
                        )?;
                    }
                }
            }
            self.emit_actor_dispatch(actor)?;
        }
        Ok(())
    }

    fn emit_actor_state_callbacks(&self, actor: &SemActor) -> CodegenResult<()> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let recipe = self
            .module
            .actor_recipes
            .get(&actor.state_ty)
            .ok_or_else(|| CodegenError::FailClosed("actor state lacks its value recipe".into()))?;
        let layout = self
            .module
            .target
            .layout(&actor.state_ty)
            .ok_or_else(|| CodegenError::FailClosed("actor state lacks its layout".into()))?;
        let drop = self.llvm.add_function(
            &symbol(actor.id, "state_drop"),
            self.ctx.void_type().fn_type(&[ptr.into()], false),
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(drop, "entry"));
        if let Some(action) = recipe.destroy {
            let loaded = builder
                .build_load(
                    llvm_type(self.ctx, &layout.repr)?,
                    drop.get_first_param().unwrap().into_pointer_value(),
                    "actor.state",
                )
                .llvm_ctx("load initialized actor state")?;
            ValueEmitter {
                module: self.module,
                ctx: self.ctx,
                llvm: &self.llvm,
                builder: &builder,
                value: drop,
            }
            .destroy_loaded_value(loaded, layout, action)?;
        }
        builder
            .build_return(None)
            .llvm_ctx("finish actor state destruction")?;
        let clone = self.llvm.add_function(
            &symbol(actor.id, "state_clone"),
            ptr.fn_type(&[ptr.into()], false),
            Some(Linkage::Internal),
        );
        builder.position_at_end(self.ctx.append_basic_block(clone, "entry"));
        if let Some(action) = recipe.clone {
            let loaded = builder
                .build_load(
                    llvm_type(self.ctx, &layout.repr)?,
                    clone.get_first_param().unwrap().into_pointer_value(),
                    "actor.state",
                )
                .llvm_ctx("load actor state snapshot source")?;
            let copied = ValueEmitter {
                module: self.module,
                ctx: self.ctx,
                llvm: &self.llvm,
                builder: &builder,
                value: clone,
            }
            .clone_loaded_value(loaded, layout, action)?;
            let allocation = allocate(self.module, self.ctx, &self.llvm, &builder, layout.size)?;
            builder
                .build_store(allocation, copied)
                .llvm_ctx("initialize independent actor snapshot")?;
            builder
                .build_return(Some(&allocation))
                .llvm_ctx("return actor state snapshot")?;
        } else {
            builder
                .build_return(Some(&ptr.const_null()))
                .llvm_ctx("refuse copying non-copyable actor state")?;
        }
        Ok(())
    }

    fn emit_actor_message_drop(
        &self,
        actor: &SemActor,
        handler: &SemActorHandler,
    ) -> CodegenResult<()> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let message_ty = message_type(self.module, self.ctx, handler)?;
        let function = self.llvm.add_function(
            &message_symbol(actor.id, handler.message_id),
            self.ctx.void_type().fn_type(&[ptr.into()], false),
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(function, "entry"));
        let payload = function.get_first_param().unwrap().into_pointer_value();
        let active = builder
            .build_load(self.ctx.i8_type(), payload, "message.active")
            .llvm_ctx("read message ownership")?
            .into_int_value();
        let destroy = self.ctx.append_basic_block(function, "destroy");
        let done = self.ctx.append_basic_block(function, "done");
        let live = builder
            .build_int_compare(
                IntPredicate::NE,
                active,
                self.ctx.i8_type().const_zero(),
                "message.live",
            )
            .llvm_ctx("test queued message owner")?;
        builder
            .build_conditional_branch(live, destroy, done)
            .llvm_ctx("drop only unconsumed message payload")?;
        builder.position_at_end(destroy);
        builder
            .build_store(payload, self.ctx.i8_type().const_zero())
            .llvm_ctx("consume queued message owner")?;
        for (index, ty) in handler.params.iter().enumerate().rev() {
            let recipe = &self.module.actor_recipes[ty];
            if let Some(action) = recipe.destroy {
                let layout = self.module.target.layout(ty).ok_or_else(|| {
                    CodegenError::FailClosed("message drop lacks field layout".into())
                })?;
                let slot = builder
                    .build_struct_gep(
                        message_ty,
                        payload,
                        u32::try_from(index + 1).map_err(|_| {
                            CodegenError::FailClosed("message field index exceeds u32".into())
                        })?,
                        "message.field",
                    )
                    .llvm_ctx("address queued field")?;
                let loaded = builder
                    .build_load(llvm_type(self.ctx, &layout.repr)?, slot, "message.value")
                    .llvm_ctx("load queued field")?;
                ValueEmitter {
                    module: self.module,
                    ctx: self.ctx,
                    llvm: &self.llvm,
                    builder: &builder,
                    value: function,
                }
                .destroy_loaded_value(loaded, layout, action)?;
            }
        }
        builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish queued message destruction")?;
        builder.position_at_end(done);
        builder
            .build_return(None)
            .llvm_ctx("return from queued message drop")?;
        Ok(())
    }

    fn emit_actor_dispatch(&self, actor: &SemActor) -> CodegenResult<()> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let target = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let dispatch = self.llvm.add_function(
            &symbol(actor.id, "dispatch"),
            ptr.fn_type(
                &[
                    ptr.into(),
                    ptr.into(),
                    self.ctx.i32_type().into(),
                    ptr.into(),
                    size_ty.into(),
                    self.ctx.i32_type().into(),
                ],
                false,
            ),
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(dispatch, "entry"));
        let ctx = dispatch.get_nth_param(0).unwrap().into_pointer_value();
        let state = dispatch.get_nth_param(1).unwrap().into_pointer_value();
        let message = dispatch.get_nth_param(2).unwrap().into_int_value();
        let payload = dispatch.get_nth_param(3).unwrap().into_pointer_value();
        let fault = builder
            .build_alloca(ptr, "handler.fault")
            .llvm_ctx("allocate synchronous handler fault")?;
        builder
            .build_store(fault, ptr.const_null())
            .llvm_ctx("initialize handler fault")?;
        let done = self.ctx.append_basic_block(dispatch, "done");
        let unknown = self.ctx.append_basic_block(dispatch, "unknown.message");
        let handlers: Vec<_> = actor
            .handlers
            .iter()
            .map(|handler| {
                (
                    handler,
                    self.ctx.append_basic_block(dispatch, &handler.name),
                )
            })
            .collect();
        let cases: Vec<_> = handlers
            .iter()
            .map(|(handler, block)| {
                (
                    self.ctx
                        .i32_type()
                        .const_int(u64::from(handler.message_id), false),
                    *block,
                )
            })
            .collect();
        builder
            .build_switch(message, unknown, &cases)
            .llvm_ctx("dispatch exact actor protocol")?;
        builder.position_at_end(unknown);
        let create_fault = external_fault_new(self.ctx, &self.llvm)?;
        let invalid = builder
            .build_call(
                create_fault,
                &[self.ctx.i32_type().const_int(1, false).into()],
                "protocol.fault",
            )
            .llvm_ctx("create invalid protocol fault")?
            .try_as_basic_value()
            .basic()
            .unwrap();
        builder
            .build_store(fault, invalid)
            .llvm_ctx("retain protocol fault")?;
        builder
            .build_unconditional_branch(done)
            .llvm_ctx("complete invalid protocol dispatch")?;
        for (handler, block) in handlers {
            builder.position_at_end(block);
            let callable = callable(self.module, handler.callable)?;
            if callable.is_resumable {
                let ramp = self.emit_actor_handler_ramp(actor, handler)?;
                let handle = call_value(
                    &builder,
                    ramp,
                    &[state.into(), payload.into()],
                    "handler.frame",
                )?
                .into_pointer_value();
                let is_done = coro::external(
                    &self.llvm,
                    "hew_cont_done",
                    self.ctx.bool_type().fn_type(&[ptr.into()], false),
                )?;
                let complete = call_value(&builder, is_done, &[handle.into()], "handler.done")?
                    .into_int_value();
                let ready = self.ctx.append_basic_block(dispatch, "handler.ready");
                let pending = self.ctx.append_basic_block(dispatch, "handler.pending");
                let finished = builder
                    .build_int_compare(
                        IntPredicate::NE,
                        complete,
                        self.ctx.bool_type().const_zero(),
                        "handler.complete",
                    )
                    .llvm_ctx("test actor turn completion")?;
                builder
                    .build_conditional_branch(finished, ready, pending)
                    .llvm_ctx("select actor turn completion")?;
                builder.position_at_end(pending);
                builder
                    .build_return(Some(&handle))
                    .llvm_ctx("park strict actor turn")?;
                builder.position_at_end(ready);
                let destroy = external_drop(self.ctx, &self.llvm, "hew_cont_destroy")?;
                builder
                    .build_call(destroy, &[handle.into()], "")
                    .llvm_ctx("destroy completed actor adapter")?;
                builder
                    .build_return(Some(&ptr.const_null()))
                    .llvm_ctx("finish ready actor turn")?;
                continue;
            }
            let message_ty = message_type(self.module, self.ctx, handler)?;
            let mut args: Vec<BasicMetadataValueEnum<'ctx>> = vec![state.into()];
            for (index, parameter) in callable.params.iter().skip(1).enumerate() {
                let slot = builder
                    .build_struct_gep(
                        message_ty,
                        payload,
                        u32::try_from(index + 1).map_err(|_| {
                            CodegenError::FailClosed("message index exceeds u32".into())
                        })?,
                        "handler.argument",
                    )
                    .llvm_ctx("address handler argument")?;
                args.push(match parameter.carrier {
                    ParamCarrier::Indirect => slot.into(),
                    ParamCarrier::Direct => builder
                        .build_load(
                            llvm_type(self.ctx, &parameter.layout.repr)?,
                            slot,
                            "handler.value",
                        )
                        .llvm_ctx("load handler argument")?
                        .into(),
                });
            }
            // Every field now transfers to the private body's cleanup graph.
            builder
                .build_store(payload, self.ctx.i8_type().const_zero())
                .llvm_ctx("transfer message fields to handler")?;
            let output = callable
                .return_layout
                .as_ref()
                .map(|layout| {
                    builder
                        .build_alloca(llvm_type(self.ctx, &layout.repr)?, "handler.reply")
                        .llvm_ctx("allocate handler reply")
                })
                .transpose()?;
            if let Some(output) = output {
                args.push(output.into());
            }
            args.push(fault.into());
            builder
                .build_call(self.functions[&handler.callable], &args, "handler.status")
                .llvm_ctx("call checked actor body")?;
            self.emit_actor_reply(&builder, dispatch, actor, handler, output, fault)?;
            builder
                .build_unconditional_branch(done)
                .llvm_ctx("complete actor handler")?;
        }
        builder.position_at_end(done);
        let set_fault = get_or_declare_external(
            &self.llvm,
            "hew_actor_dispatch_set_fault",
            self.ctx
                .void_type()
                .fn_type(&[ptr.into(), ptr.into()], false),
        )?;
        let returned_fault = builder
            .build_load(ptr, fault, "returned.fault")
            .llvm_ctx("load actor dispatch fault")?;
        builder
            .build_call(set_fault, &[ctx.into(), returned_fault.into()], "")
            .llvm_ctx("transfer handler fault to scheduler")?;
        builder
            .build_return(Some(&ptr.const_null()))
            .llvm_ctx("complete synchronous strict actor turn")?;
        Ok(())
    }

    fn emit_actor_handler_ramp(
        &self,
        actor: &SemActor,
        handler: &SemActorHandler,
    ) -> CodegenResult<FunctionValue<'ctx>> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let ramp = self.llvm.add_function(
            &symbol(actor.id, &format!("handler_{}_start", handler.message_id)),
            ptr.fn_type(&[ptr.into(), ptr.into()], false),
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(ramp, "entry"));
        let new_state = coro::external(
            &self.llvm,
            "hew_actor_coro_state_new",
            ptr.fn_type(&[], false),
        )?;
        let child =
            call_value(&builder, new_state, &[], "handler.invocation")?.into_pointer_value();
        let frame = coro::begin(self.ctx, &self.llvm, &builder, ramp, child)?;
        let fault = builder
            .build_alloca(ptr, "handler.fault")
            .llvm_ctx("allocate persistent handler fault")?;
        builder
            .build_store(fault, ptr.const_null())
            .llvm_ctx("initialize handler fault")?;
        let callable = callable(self.module, handler.callable)?;
        let output = callable
            .return_layout
            .as_ref()
            .map(|layout| {
                builder
                    .build_alloca(llvm_type(self.ctx, &layout.repr)?, "handler.reply")
                    .llvm_ctx("allocate persistent handler reply")
            })
            .transpose()?;
        let state = ramp.get_nth_param(0).unwrap().into_pointer_value();
        let payload = ramp.get_nth_param(1).unwrap().into_pointer_value();
        let message_ty = message_type(self.module, self.ctx, handler)?;
        let mut args: Vec<BasicMetadataValueEnum<'ctx>> = vec![state.into()];
        for (index, parameter) in callable.params.iter().skip(1).enumerate() {
            let slot = builder
                .build_struct_gep(message_ty, payload, (index + 1) as u32, "handler.argument")
                .llvm_ctx("address handler argument")?;
            args.push(match parameter.carrier {
                ParamCarrier::Indirect => slot.into(),
                ParamCarrier::Direct => builder
                    .build_load(
                        llvm_type(self.ctx, &parameter.layout.repr)?,
                        slot,
                        "handler.value",
                    )
                    .llvm_ctx("load handler argument")?
                    .into(),
            });
        }
        builder
            .build_store(payload, self.ctx.i8_type().const_zero())
            .llvm_ctx("transfer message fields to handler frame")?;
        if let Some(output) = output {
            args.push(output.into());
        }
        args.push(fault.into());
        args.push(child.into());
        let child_frame = call_value(
            &builder,
            self.ramps[&handler.callable],
            &args,
            "handler.body.frame",
        )?
        .into_pointer_value();
        suspend::await_child(
            self.ctx,
            &self.llvm,
            &builder,
            ramp,
            &frame,
            child,
            child_frame,
        )?;
        self.emit_actor_reply(&builder, ramp, actor, handler, output, fault)?;
        let returned_fault = builder
            .build_load(ptr, fault, "handler.returned.fault")
            .llvm_ctx("read completed handler fault")?;
        let publish = coro::external(
            &self.llvm,
            "hew_actor_coro_set_fault",
            self.ctx.void_type().fn_type(&[ptr.into()], false),
        )?;
        builder
            .build_call(publish, &[returned_fault.into()], "")
            .llvm_ctx("publish handler completion under current activation")?;
        builder
            .build_unconditional_branch(frame.finish)
            .llvm_ctx("finish actor adapter frame")?;
        Ok(ramp)
    }
}

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    pub(super) fn emit_actor_call(
        &self,
        operation: ActorOperation,
        transfers: &[ArgumentTransfer],
        result: Option<StorageId>,
        normal: &PhysicalEdge,
        unwind: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        let id = match &operation {
            ActorOperation::Spawn(id) | ActorOperation::Submit { actor: id, .. } => *id,
        };
        let actor =
            self.module.actors.get(id.0 as usize).ok_or_else(|| {
                CodegenError::FailClosed("missing native actor descriptor".into())
            })?;
        let mut sources = Vec::new();
        for transfer in transfers {
            let ArgumentTransfer::Move(source) = transfer else {
                return Err(CodegenError::FailClosed(
                    "actor boundary lacks payload transfer".into(),
                ));
            };
            sources.push(*source);
        }
        let status = match operation {
            ActorOperation::Spawn(_) => self.emit_actor_spawn(actor, &sources, result)?,
            ActorOperation::Submit {
                policy,
                message_ty,
                result_ty,
                ..
            } => {
                let [source] = sources.as_slice() else {
                    return Err(CodegenError::FailClosed(
                        "submission requires one message owner".into(),
                    ));
                };
                let result = result.ok_or_else(|| {
                    CodegenError::FailClosed("submission requires its typed result".into())
                })?;
                self.emit_actor_submit(actor, policy, &message_ty, &result_ty, *source, result)?;
                self.ctx.i32_type().const_zero()
            }
        };
        for source in sources {
            self.clear_owned(source)?;
        }
        self.builder
            .build_store(self.active_status, status)
            .llvm_ctx("record actor boundary status")?;
        self.emit_call_outcome(status, result, normal, unwind)
    }

    fn emit_actor_spawn(
        &self,
        actor: &SemActor,
        sources: &[StorageId],
        result: Option<StorageId>,
    ) -> CodegenResult<IntValue<'ctx>> {
        let result = result
            .ok_or_else(|| CodegenError::FailClosed("spawn lacks stable handle output".into()))?;
        let layout = self
            .module
            .target
            .layout(&actor.state_ty)
            .ok_or_else(|| CodegenError::FailClosed("missing actor state layout".into()))?;
        let state = allocate(self.module, self.ctx, self.llvm, &self.builder, layout.size)?;
        for (index, source) in sources.iter().take(actor.fields.len()).enumerate() {
            let field = self
                .builder
                .build_struct_gep(
                    llvm_type(self.ctx, &layout.repr)?.into_struct_type(),
                    state,
                    u32::try_from(index)
                        .map_err(|_| CodegenError::FailClosed("actor field exceeds u32".into()))?,
                    "spawn.field",
                )
                .llvm_ctx("address initial actor field")?;
            self.builder
                .build_store(field, self.load(*source, "spawn.value")?)
                .llvm_ctx("initialize actor field")?;
        }
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let init_failure = if let Some(init) = actor.init {
            let callable = callable(self.module, init)?;
            let mut arguments: Vec<BasicMetadataValueEnum<'ctx>> = vec![state.into()];
            for (source, parameter) in sources
                .iter()
                .skip(actor.fields.len())
                .zip(callable.params.iter().skip(1))
            {
                arguments.push(match parameter.carrier {
                    ParamCarrier::Direct => self.load(*source, "init.argument")?.into(),
                    ParamCarrier::Indirect => self.slots[source.0 as usize].into(),
                });
            }
            arguments.push(self.active_fault.into());
            let status = self
                .builder
                .build_call(self.functions[&init], &arguments, "actor.init.status")
                .llvm_ctx("initialize actor before publication")?
                .try_as_basic_value()
                .basic()
                .unwrap()
                .into_int_value();
            let initialized = self.ctx.append_basic_block(self.value, "actor.initialized");
            let failed = self.ctx.append_basic_block(self.value, "actor.init.failed");
            let ok = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    status,
                    self.ctx.i32_type().const_zero(),
                    "actor.init.ok",
                )
                .llvm_ctx("check actor initialization")?;
            self.builder
                .build_conditional_branch(ok, initialized, failed)
                .llvm_ctx("publish only successfully initialized actors")?;
            self.builder.position_at_end(failed);
            let drop = self
                .llvm
                .get_function(&symbol(actor.id, "state_drop"))
                .ok_or_else(|| {
                    CodegenError::FailClosed("actor init cleanup lacks state destructor".into())
                })?;
            self.builder
                .build_call(drop, &[state.into()], "")
                .llvm_ctx("destroy unpublished actor state")?;
            let free = external_drop(self.ctx, self.llvm, "free")?;
            self.builder
                .build_call(free, &[state.into()], "")
                .llvm_ctx("free unpublished actor state")?;
            let joined = self
                .ctx
                .append_basic_block(self.value, "actor.spawn.complete");
            self.builder
                .build_unconditional_branch(joined)
                .llvm_ctx("propagate actor init failure")?;
            self.builder.position_at_end(initialized);
            Some((status, failed, joined))
        } else {
            None
        };
        let target = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let spawn = get_or_declare_external(
            self.llvm,
            "hew_actor_spawn_native",
            size_ty.fn_type(
                &[
                    ptr.into(),
                    size_ty.into(),
                    ptr.into(),
                    ptr.into(),
                    ptr.into(),
                    self.ctx.i32_type().into(),
                    self.ctx.i32_type().into(),
                    size_ty.into(),
                    ptr.into(),
                ],
                false,
            ),
        )?;
        let callback = |suffix| {
            self.llvm
                .get_function(&symbol(actor.id, suffix))
                .map(|function| function.as_global_value().as_pointer_value())
                .ok_or_else(|| CodegenError::FailClosed("missing actor callback".into()))
        };
        let overflow = match actor.overflow {
            hew_mir::physical::SemActorOverflow::Block => 0,
            hew_mir::physical::SemActorOverflow::DropNew => 1,
            hew_mir::physical::SemActorOverflow::DropOld => 2,
            hew_mir::physical::SemActorOverflow::Fail => 3,
        };
        let token = self
            .builder
            .build_call(
                spawn,
                &[
                    state.into(),
                    size_ty.const_int(layout.size, false).into(),
                    callback("dispatch")?.into(),
                    callback("state_drop")?.into(),
                    callback("state_clone")?.into(),
                    self.ctx
                        .i32_type()
                        .const_int(u64::from(actor.mailbox_capacity.unwrap_or(0)), false)
                        .into(),
                    self.ctx.i32_type().const_int(overflow, false).into(),
                    size_ty
                        .const_int(actor.max_heap_bytes.unwrap_or(0), false)
                        .into(),
                    self.active_fault.into(),
                ],
                "spawn.token",
            )
            .llvm_ctx("publish initialized actor")?
            .try_as_basic_value()
            .basic()
            .unwrap()
            .into_int_value();
        self.store(result, token.into())?;
        let failed = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                token,
                size_ty.const_zero(),
                "spawn.failed",
            )
            .llvm_ctx("check actor publication")?;
        let status = self
            .builder
            .build_select(
                failed,
                self.ctx.i32_type().const_int(1, false),
                self.ctx.i32_type().const_zero(),
                "spawn.status",
            )
            .llvm_ctx("select actor publication status")?
            .into_int_value();
        if let Some((init_status, failed, joined)) = init_failure {
            let published = self.builder.get_insert_block().ok_or_else(|| {
                CodegenError::FailClosed("actor publication has no LLVM block".into())
            })?;
            self.builder
                .build_unconditional_branch(joined)
                .llvm_ctx("join actor publication outcome")?;
            self.builder.position_at_end(joined);
            let result = self
                .builder
                .build_phi(self.ctx.i32_type(), "actor.spawn.status")
                .llvm_ctx("join initialization and publication statuses")?;
            result.add_incoming(&[(&init_status, failed), (&status, published)]);
            Ok(result.as_basic_value().into_int_value())
        } else {
            Ok(status)
        }
    }

    fn emit_actor_submit(
        &self,
        actor: &SemActor,
        policy: hew_types::actor_delivery::SendPolicy,
        message_ty: &ResolvedTy,
        result_ty: &ResolvedTy,
        source: StorageId,
        destination: StorageId,
    ) -> CodegenResult<()> {
        use hew_types::actor_delivery::SendPolicy;
        if matches!(policy, SendPolicy::Wait | SendPolicy::ReplaceLatest) {
            return Err(CodegenError::FailClosed(
                "submission requires its checked readiness or coalescing contract".into(),
            ));
        }
        let ResolvedTy::Named { args, .. } = message_ty else {
            return Err(CodegenError::FailClosed(
                "message has no typed payload".into(),
            ));
        };
        let Some(ResolvedTy::Tuple(params)) = args.get(1) else {
            return Err(CodegenError::FailClosed(
                "message payload is not a protocol tuple".into(),
            ));
        };
        let handler = actor
            .handlers
            .iter()
            .find(|handler| handler.return_ty == ResolvedTy::Unit && handler.params == *params)
            .ok_or_else(|| {
                CodegenError::FailClosed(
                    "message payload has no exact actor protocol signature".into(),
                )
            })?;
        let object = self.load(source, "submission.message")?.into_struct_value();
        let target = self
            .builder
            .build_extract_value(object, 0, "submission.target")
            .llvm_ctx("read message target")?;
        let member = self
            .builder
            .build_extract_value(object, 1, "submission.member")
            .llvm_ctx("read message member")?;
        let fields = self
            .builder
            .build_extract_value(object, 2, "submission.payload")
            .llvm_ctx("read message payload")?
            .into_struct_value();
        let wrapper_ty = message_type(self.module, self.ctx, handler)?;
        let target_data = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target_data, None);
        let size = target_data.get_abi_size(&wrapper_ty);
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let allocate = get_or_declare_external(
            self.llvm,
            "hew_actor_payload_try_alloc",
            ptr.fn_type(&[size_ty.into()], false),
        )?;
        let wrapper = self
            .builder
            .build_call(
                allocate,
                &[size_ty.const_int(size, false).into()],
                "submission.allocate",
            )
            .llvm_ctx("allocate unpublished message wrapper")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("message allocation returned void".into()))?
            .into_pointer_value();
        let allocated = self
            .ctx
            .append_basic_block(self.value, "submission.allocated");
        let oom = self.ctx.append_basic_block(self.value, "submission.oom");
        let submitted = self.ctx.append_basic_block(self.value, "submission.result");
        let failed = self
            .builder
            .build_is_null(wrapper, "submission.no_memory")
            .llvm_ctx("check message allocation")?;
        self.builder
            .build_conditional_branch(failed, oom, allocated)
            .llvm_ctx("branch on message allocation")?;
        self.builder.position_at_end(oom);
        self.builder
            .build_unconditional_branch(submitted)
            .llvm_ctx("return allocation failure")?;
        self.builder.position_at_end(allocated);
        self.builder
            .build_store(wrapper, self.ctx.i8_type().const_int(1, false))
            .llvm_ctx("initialize message ownership")?;
        for index in 0..params.len() {
            let index = u32::try_from(index)
                .map_err(|_| CodegenError::FailClosed("message index exceeds u32".into()))?;
            let slot = self
                .builder
                .build_struct_gep(wrapper_ty, wrapper, index + 1, "submission.field")
                .llvm_ctx("address message field")?;
            let field = self
                .builder
                .build_extract_value(fields, index, "submission.value")
                .llvm_ctx("read message field")?;
            self.builder
                .build_store(slot, field)
                .llvm_ctx("transfer field into unpublished wrapper")?;
        }
        let submit = get_or_declare_external(
            self.llvm,
            "hew_actor_submit_native",
            self.ctx.i32_type().fn_type(
                &[
                    size_ty.into(),
                    self.ctx.i32_type().into(),
                    ptr.into(),
                    size_ty.into(),
                    ptr.into(),
                    self.ctx.i32_type().into(),
                ],
                false,
            ),
        )?;
        let drop = self
            .llvm
            .get_function(&message_symbol(actor.id, handler.message_id))
            .ok_or_else(|| {
                CodegenError::FailClosed("message lacks its exact payload destructor".into())
            })?;
        let policy = if policy == SendPolicy::DropNewest {
            2
        } else {
            0
        };
        let status = self
            .builder
            .build_call(
                submit,
                &[
                    target.into(),
                    member.into(),
                    wrapper.into(),
                    size_ty.const_int(size, false).into(),
                    drop.as_global_value().as_pointer_value().into(),
                    self.ctx.i32_type().const_int(policy, false).into(),
                ],
                "submission.status",
            )
            .llvm_ctx("attempt message admission")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("submission returned void".into()))?
            .into_int_value();
        self.builder
            .build_unconditional_branch(submitted)
            .llvm_ctx("join admission outcome")?;
        self.builder.position_at_end(submitted);
        let outcome = self
            .builder
            .build_phi(self.ctx.i32_type(), "submission.outcome")
            .llvm_ctx("join submission status")?;
        outcome.add_incoming(&[
            (&self.ctx.i32_type().const_int(3, false), oom),
            (&status, allocated),
        ]);
        self.write_actor_delivery_result(
            outcome.as_basic_value().into_int_value(),
            object,
            result_ty,
            destination,
        )
    }

    fn write_actor_delivery_result(
        &self,
        status: IntValue<'ctx>,
        message: inkwell::values::StructValue<'ctx>,
        result_ty: &ResolvedTy,
        destination: StorageId,
    ) -> CodegenResult<()> {
        let result = self
            .module
            .variant_glue
            .iter()
            .find(|glue| glue.ty == *result_ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("submission result has no variant recipe".into())
            })?;
        let delivery_ty = &result.variants[0].fields[0].ty;
        let failure_ty = &result.variants[1].fields[0].ty;
        let failure = self
            .module
            .aggregate_glue
            .iter()
            .find(|glue| glue.ty == *failure_ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("rejected message has no record recipe".into())
            })?;
        let error_ty = &failure.fields[0].ty;
        let accepted = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_zero(),
                "submission.accepted",
            )
            .llvm_ctx("test acceptance")?;
        let discarded = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_int(4, false),
                "submission.discarded",
            )
            .llvm_ctx("test explicit discard")?;
        let succeeded = self
            .builder
            .build_or(accepted, discarded, "submission.success")
            .llvm_ctx("combine successful delivery outcomes")?;
        let success = self
            .ctx
            .append_basic_block(self.value, "submission.success");
        let rejected = self
            .ctx
            .append_basic_block(self.value, "submission.rejected");
        let done = self.ctx.append_basic_block(self.value, "submission.done");
        self.builder
            .build_conditional_branch(succeeded, success, rejected)
            .llvm_ctx("preserve message on rejection")?;
        self.builder.position_at_end(success);
        let delivery_tag = self
            .builder
            .build_int_z_extend(discarded, self.ctx.i8_type(), "delivery.tag")
            .llvm_ctx("select delivery variant")?;
        let delivery = self.actor_unit_variant(delivery_ty, delivery_tag)?;
        self.write_variant_value(
            self.slots[destination.0 as usize],
            0,
            &[delivery],
            result.id,
        )?;
        self.builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish successful submission")?;
        self.builder.position_at_end(rejected);
        let closed = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_int(2, false),
                "submission.closed",
            )
            .llvm_ctx("classify closed destination")?;
        let full = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_int(1, false),
                "submission.full",
            )
            .llvm_ctx("classify full mailbox")?;
        let reason = self
            .builder
            .build_select(
                closed,
                self.ctx.i8_type().const_int(1, false),
                self.ctx.i8_type().const_int(9, false),
                "submission.reason",
            )
            .llvm_ctx("classify admission failure")?;
        let reason = self
            .builder
            .build_select(
                full,
                self.ctx.i8_type().const_zero(),
                reason.into_int_value(),
                "submission.full_reason",
            )
            .llvm_ctx("classify full rejection")?
            .into_int_value();
        let reason = self.actor_unit_variant(error_ty, reason)?;
        let failure_layout = self.module.target.layout(failure_ty).ok_or_else(|| {
            CodegenError::FailClosed("send failure lacks its target layout".into())
        })?;
        let failure_object = llvm_type(self.ctx, &failure_layout.repr)?
            .into_struct_type()
            .const_zero();
        let failure_object = self
            .builder
            .build_insert_value(failure_object, reason, 0, "submission.failure_reason")
            .llvm_ctx("store rejection reason")?;
        let failure_object = self
            .builder
            .build_insert_value(failure_object, message, 1, "submission.returned_message")
            .llvm_ctx("return complete rejected message")?
            .into_struct_value();
        self.write_variant_value(
            self.slots[destination.0 as usize],
            1,
            &[failure_object.into()],
            result.id,
        )?;
        self.builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish rejected submission")?;
        self.builder.position_at_end(done);
        Ok(())
    }

    pub(super) fn actor_unit_variant(
        &self,
        ty: &ResolvedTy,
        tag: IntValue<'ctx>,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let glue = self
            .module
            .variant_glue
            .iter()
            .find(|glue| glue.ty == *ty)
            .filter(|glue| {
                glue.variants
                    .iter()
                    .all(|variant| variant.fields.is_empty())
            })
            .ok_or_else(|| {
                CodegenError::FailClosed("delivery status requires a unit-only enum".into())
            })?;
        let layout = self.value_emitter().variant_layout(&glue.ty)?;
        let object = llvm_type(self.ctx, &layout.object.repr)?
            .into_struct_type()
            .const_zero();
        let tag = self
            .builder
            .build_int_cast(
                tag,
                object
                    .get_type()
                    .get_field_type_at_index(0)
                    .unwrap()
                    .into_int_type(),
                "actor.error.tag",
            )
            .llvm_ctx("materialize the enum's physical discriminator")?;
        Ok(self
            .builder
            .build_insert_value(object, tag, 0, "submission.unit_variant")
            .llvm_ctx("construct delivery status")?
            .into_struct_value()
            .into())
    }
}
