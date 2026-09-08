//! Native actor adapters over verified state, message and callable contracts.

use super::suspend::call_value;
use super::*;
use hew_mir::physical::{ActorId, ActorOperation, SemActor, SemActorHandler, SemFailureDisplay};
use inkwell::types::StructType;

#[path = "physical_actor_ask.rs"]
mod ask;
#[path = "physical_actor_lifecycle.rs"]
mod lifecycle;

#[path = "physical_actor_wait.rs"]
mod wait;

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
        fault_slot: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        // A void handler replies too: its completion is the unit reply a
        // completion call waits for. `hew_actor_reply_native` discards the
        // reply when the message arrived through a one-way mailbox view and
        // carries no channel.
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let target = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let complete = self.ctx.append_basic_block(function, "reply.complete");
        let publish = self.ctx.append_basic_block(function, "reply.publish");
        let fault = builder
            .build_load(ptr, fault_slot, "reply.fault")
            .llvm_ctx("inspect handler completion")?
            .into_pointer_value();
        let success = builder
            .build_is_null(fault, "reply.success")
            .llvm_ctx("check successful reply")?;
        builder
            .build_conditional_branch(success, publish, complete)
            .llvm_ctx("publish only an initialized reply")?;
        builder.position_at_end(publish);
        // A `fails` handler submitted one way through a mailbox view has no
        // caller to receive its declared error, so the error becomes this
        // actor's own fault instead of being discarded with the reply.
        if let (Some(display), Some(output)) = (handler.failure_display, output) {
            let reply = self.ctx.append_basic_block(function, "reply.transfer");
            self.emit_unhandled_failure(
                builder, function, actor, handler, output, fault_slot, display, reply, complete,
            )?;
            builder.position_at_end(reply);
        }
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

    /// Raise a `fails` handler's declared error as this actor's own fault when
    /// the submission carried no reply channel. The caller positions the
    /// builder in the reply-publishing block; on return control reaches
    /// `transfer` for every completion that still owes a reply.
    #[allow(
        clippy::too_many_arguments,
        reason = "one failure raise owns the reply slot, the fault seat and its rendering"
    )]
    fn emit_unhandled_failure(
        &self,
        builder: &Builder<'ctx>,
        function: FunctionValue<'ctx>,
        actor: &SemActor,
        handler: &SemActorHandler,
        output: PointerValue<'ctx>,
        fault_slot: PointerValue<'ctx>,
        display: SemFailureDisplay,
        transfer: BasicBlock<'ctx>,
        complete: BasicBlock<'ctx>,
    ) -> CodegenResult<()> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let values = ValueEmitter {
            module: self.module,
            ctx: self.ctx,
            llvm: &self.llvm,
            builder,
            value: function,
        };
        let inspect = self.ctx.append_basic_block(function, "reply.oneway");
        let raise = self.ctx.append_basic_block(function, "reply.unhandled");
        let channel = coro::external(&self.llvm, "hew_get_reply_channel", ptr.fn_type(&[], false))?;
        let channel = call_value(builder, channel, &[], "reply.channel")?.into_pointer_value();
        let one_way = builder
            .build_is_null(channel, "reply.oneway.test")
            .llvm_ctx("classify a one-way submission")?;
        builder
            .build_conditional_branch(one_way, inspect, transfer)
            .llvm_ctx("select the one-way completion")?;
        builder.position_at_end(inspect);
        let layout = values.variant_layout(&handler.return_ty)?;
        let object = values.variant_object_ptr(output, layout)?;
        let tag = values.load_variant_tag(object, layout)?;
        let succeeded = builder
            .build_int_compare(
                IntPredicate::EQ,
                tag,
                tag.get_type().const_zero(),
                "reply.declared.ok",
            )
            .llvm_ctx("classify the declared handler outcome")?;
        builder
            .build_conditional_branch(succeeded, transfer, raise)
            .llvm_ctx("raise only a declared failure")?;
        builder.position_at_end(raise);
        let payload_ty = llvm_type(self.ctx, &layout.variants[1].repr)?.into_struct_type();
        let payload = builder
            .build_load(
                payload_ty,
                values.variant_payload_ptr(object, layout)?,
                "reply.declared.payload",
            )
            .llvm_ctx("read the declared failure payload")?
            .into_struct_value();
        let error = builder
            .build_extract_value(payload, 0, "reply.declared.error")
            .llvm_ctx("take the declared failure")?;
        // `Identity` means the error is already its text and stays owned by the
        // reply; a rendered error is a fresh string this block owns.
        let (text, release_text, drop_reply) = match display {
            SemFailureDisplay::Identity => (error.into_pointer_value(), false, true),
            SemFailureDisplay::Callable(id) => {
                let render = callable(self.module, id)?;
                let parameter = render.params.first().ok_or_else(|| {
                    CodegenError::FailClosed("declared failure Display takes no receiver".into())
                })?;
                let out = builder
                    .build_alloca(ptr, "reply.declared.text")
                    .llvm_ctx("allocate the rendered failure")?;
                let argument: BasicMetadataValueEnum<'ctx> = match parameter.carrier {
                    ParamCarrier::Direct => error.into(),
                    ParamCarrier::Indirect => {
                        let slot = builder
                            .build_alloca(
                                llvm_type(self.ctx, &parameter.layout.repr)?,
                                "reply.declared.arg",
                            )
                            .llvm_ctx("allocate the failure Display argument")?;
                        builder
                            .build_store(slot, error)
                            .llvm_ctx("place the failure Display argument")?;
                        slot.into()
                    }
                };
                builder
                    .build_call(
                        self.functions[&id],
                        &[argument, out.into(), fault_slot.into()],
                        "reply.render",
                    )
                    .llvm_ctx("render the declared failure")?;
                let rendered = builder
                    .build_load(ptr, out, "reply.declared.rendered")
                    .llvm_ctx("take the rendered failure")?
                    .into_pointer_value();
                // A consuming renderer took the payload with it; only a
                // borrowing one leaves the reply for its own drop glue.
                let borrows = !matches!(
                    parameter.passing,
                    hew_mir::physical::SemParamPassing::Consume
                );
                (rendered, true, borrows)
            }
        };
        // A renderer that faulted owns the diagnostic; its fault is already in
        // the seat and the rendered text was never produced.
        let rendered = self.ctx.append_basic_block(function, "reply.rendered");
        let faulted = builder
            .build_load(ptr, fault_slot, "reply.render.fault")
            .llvm_ctx("inspect the renderer's completion")?
            .into_pointer_value();
        let clean = builder
            .build_is_null(faulted, "reply.render.clean")
            .llvm_ctx("check the renderer's completion")?;
        let discard = self.ctx.append_basic_block(function, "reply.discard");
        builder
            .build_conditional_branch(clean, rendered, discard)
            .llvm_ctx("keep the first fault")?;
        builder.position_at_end(rendered);
        let create = get_or_declare_external(
            &self.llvm,
            "hew_fault_new_unhandled_failure",
            ptr.fn_type(&[ptr.into()], false),
        )?;
        let raised = call_value(builder, create, &[text.into()], "reply.unhandled.fault")?;
        builder
            .build_store(fault_slot, raised)
            .llvm_ctx("retain the unhandled failure")?;
        if release_text {
            let release = external_drop(self.ctx, &self.llvm, "hew_string_drop")?;
            builder
                .build_call(release, &[text.into()], "")
                .llvm_ctx("release the rendered failure")?;
        }
        builder
            .build_unconditional_branch(discard)
            .llvm_ctx("discard the unreplied outcome")?;
        builder.position_at_end(discard);
        if drop_reply {
            if let Some(glue) = self
                .llvm
                .get_function(&reply_symbol(actor.id, handler.message_id))
            {
                builder
                    .build_call(glue, &[output.into()], "")
                    .llvm_ctx("destroy the unreplied outcome")?;
            }
        }
        builder
            .build_unconditional_branch(complete)
            .llvm_ctx("finish the unreplied completion")?;
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

pub(super) fn allocate<'ctx>(
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
    fn needs_process_runtime(&self) -> bool {
        !self.module.actors.is_empty()
            || self
                .module
                .functions
                .iter()
                .flat_map(|function| &function.blocks)
                .any(|block| matches!(
                    block.terminator,
                    PhysicalTerminator::NativeIo { .. }
                        | PhysicalTerminator::RuntimeCall {
                            action: hew_mir::physical::PhysicalRuntimeAction::NodeLifecycle { .. } | hew_mir::physical::PhysicalRuntimeAction::NodeShutdown,
                            ..
                        }
                ))
    }

    pub(super) fn emit_process_runtime_start(
        &self,
        builder: &Builder<'ctx>,
        wrapper: FunctionValue<'ctx>,
    ) -> CodegenResult<()> {
        if !self.needs_process_runtime() {
            return Ok(());
        }
        let start = get_or_declare_external(
            &self.llvm,
            "hew_sched_init",
            self.ctx.i32_type().fn_type(&[], false),
        )?;
        let status = builder
            .build_call(start, &[], "runtime.start")
            .llvm_ctx("start required process runtime")?
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

    pub(super) fn emit_process_runtime_finish(
        &self,
        builder: &Builder<'ctx>,
        status: IntValue<'ctx>,
    ) -> CodegenResult<IntValue<'ctx>> {
        if !self.needs_process_runtime() {
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
            .llvm_ctx("drain work and finish process runtime")?
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
            if actor.crash.is_some() {
                self.emit_actor_crash(actor)?;
            }
            if actor.exit.is_some() || actor.down.is_some() {
                self.emit_actor_sys_dispatch(actor)?;
            }
            if !actor.stop.is_empty() {
                self.emit_actor_terminate(actor)?;
            }
        }
        self.emit_supervisor_descriptors()
    }

    /// `#[on(stop)]` hooks run in lexical order at the terminal transition
    /// with the state still initialized. The first fault ends the sequence
    /// and becomes the actor's retained lifecycle diagnostic.
    fn emit_actor_terminate(&self, actor: &SemActor) -> CodegenResult<()> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let terminate = self.llvm.add_function(
            &symbol(actor.id, "terminate"),
            self.ctx.void_type().fn_type(&[ptr.into()], false),
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(terminate, "entry"));
        let state = terminate.get_first_param().unwrap().into_pointer_value();
        let fault = builder
            .build_alloca(ptr, "hook.fault")
            .llvm_ctx("allocate stop hook fault")?;
        builder
            .build_store(fault, ptr.const_null())
            .llvm_ctx("initialize stop hook fault")?;
        let failed = self.ctx.append_basic_block(terminate, "hook.failed");
        let done = self.ctx.append_basic_block(terminate, "done");
        for hook in &actor.stop {
            let status = builder
                .build_call(
                    self.functions[hook],
                    &[state.into(), fault.into()],
                    "hook.status",
                )
                .llvm_ctx("run stop hook")?
                .try_as_basic_value()
                .basic()
                .unwrap()
                .into_int_value();
            let next = self.ctx.append_basic_block(terminate, "hook.next");
            let ok = builder
                .build_int_compare(
                    IntPredicate::EQ,
                    status,
                    self.ctx.i32_type().const_zero(),
                    "hook.ok",
                )
                .llvm_ctx("check stop hook outcome")?;
            builder
                .build_conditional_branch(ok, next, failed)
                .llvm_ctx("continue stop hook sequence")?;
            builder.position_at_end(next);
        }
        builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish stop hooks")?;
        builder.position_at_end(failed);
        let returned = builder
            .build_load(ptr, fault, "hook.returned.fault")
            .llvm_ctx("load stop hook fault")?;
        let publish = get_or_declare_external(
            &self.llvm,
            "hew_actor_terminate_set_fault",
            self.ctx.void_type().fn_type(&[ptr.into()], false),
        )?;
        builder
            .build_call(publish, &[returned.into()], "")
            .llvm_ctx("retain stop hook fault")?;
        builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish faulted stop hooks")?;
        builder.position_at_end(done);
        builder
            .build_return(None)
            .llvm_ctx("finish actor terminate")?;
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
            // Empty timer messages have no payload or field ownership to transfer.
            if !handler.params.is_empty() {
                builder
                    .build_store(payload, self.ctx.i8_type().const_zero())
                    .llvm_ctx("transfer message fields to handler")?;
            }
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
        if !handler.params.is_empty() {
            builder
                .build_store(payload, self.ctx.i8_type().const_zero())
                .llvm_ctx("transfer message fields to handler frame")?;
        }
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
        if let ActorOperation::LocalObservation { kind, .. } = &operation {
            return self.emit_local_observation(*kind, transfers, result, normal, unwind);
        }
        match &operation {
            ActorOperation::CallStart(protocol) => {
                let result = result.ok_or_else(|| {
                    CodegenError::FailClosed("completion start has no result owner".into())
                })?;
                let value = self.emit_actor_call_start(
                    protocol.actor,
                    protocol.message,
                    protocol.policy,
                    protocol.deadline_ns,
                    protocol.sealed,
                    transfers,
                )?;
                self.store(result, value.into())?;
                return self.emit_result_edge(Some(result), normal);
            }
            ActorOperation::CallTake(protocol) => {
                let [ArgumentTransfer::Move(operation), ArgumentTransfer::Borrow(target)] =
                    transfers
                else {
                    return Err(CodegenError::FailClosed(
                        "completion take changes its owners".into(),
                    ));
                };
                let result = result.ok_or_else(|| {
                    CodegenError::FailClosed("completion take has no result".into())
                })?;
                let value = self
                    .load(*operation, "ask.selected.operation")?
                    .into_pointer_value();
                self.emit_actor_call_take(
                    value,
                    protocol.actor,
                    protocol.message,
                    protocol.policy,
                    *target,
                    result,
                )?;
                self.clear_owned(*operation)?;
                return self.emit_result_edge(Some(result), normal);
            }
            _ => {}
        }
        let id = match &operation {
            ActorOperation::LocalObservation { .. }
            | ActorOperation::CallStart(_)
            | ActorOperation::CallTake(_) => {
                unreachable!("special boundary returned above")
            }
            ActorOperation::Spawn(id)
            | ActorOperation::SelfHandle(id)
            | ActorOperation::Close(id)
            | ActorOperation::AwaitClosed(id)
            | ActorOperation::StreamStart { actor: id, .. }
            | ActorOperation::Submit { actor: id, .. } => *id,
            ActorOperation::SupervisorSpawn(_)
            | ActorOperation::SupervisorChild { .. }
            | ActorOperation::SupervisorAwaitRestart { .. }
            | ActorOperation::SupervisorAwaitClosed(_)
            | ActorOperation::SupervisorRoleAwaitClosed { .. }
            | ActorOperation::SupervisorStop(_) => ActorId(u32::MAX),
        };
        let mut sources = Vec::new();
        for transfer in transfers {
            let ArgumentTransfer::Move(source) = transfer else {
                return Err(CodegenError::FailClosed(
                    "actor boundary lacks payload transfer".into(),
                ));
            };
            sources.push(*source);
        }
        if let Some(status) = self.emit_supervisor_boundary(&operation, &sources, result)? {
            for source in sources {
                self.clear_owned(source)?;
            }
            self.builder
                .build_store(self.active_status, status)
                .llvm_ctx("record supervisor boundary status")?;
            return self.emit_call_outcome(status, result, Some(normal), unwind);
        }
        if matches!(
            operation,
            ActorOperation::AwaitClosed(_)
                | ActorOperation::SupervisorAwaitClosed(_)
                | ActorOperation::SupervisorRoleAwaitClosed { .. }
        ) {
            let [source] = sources.as_slice() else {
                return Err(CodegenError::FailClosed(
                    "termination wait requires one identity".into(),
                ));
            };
            let role_close = match operation {
                ActorOperation::SupervisorRoleAwaitClosed { closing, .. } => Some(closing),
                _ => None,
            };
            self.emit_actor_await_closed(*source, role_close, unwind)?;
            for source in sources {
                self.clear_owned(source)?;
            }
            let status = self.ctx.i32_type().const_zero();
            self.builder
                .build_store(self.active_status, status)
                .llvm_ctx("record termination observation status")?;
            return self.emit_call_outcome(status, result, Some(normal), unwind);
        }
        let actor =
            self.module.actors.get(id.0 as usize).ok_or_else(|| {
                CodegenError::FailClosed("missing native actor descriptor".into())
            })?;
        let status = match operation {
            ActorOperation::LocalObservation { .. }
            | ActorOperation::CallStart(_)
            | ActorOperation::CallTake(_) => {
                unreachable!("special boundary returned above")
            }
            ActorOperation::Close(_) => {
                let [source] = sources.as_slice() else {
                    return Err(CodegenError::FailClosed(
                        "close requires one actor identity".into(),
                    ));
                };
                let value = self.load(*source, "close.actor")?;
                let close = coro::external(
                    self.llvm,
                    "hew_actor_close_native",
                    self.ctx
                        .void_type()
                        .fn_type(&[value.get_type().into()], false),
                )?;
                self.builder
                    .build_call(close, &[value.into()], "")
                    .llvm_ctx("request cooperative actor stop")?;
                self.store(
                    result.ok_or_else(|| {
                        CodegenError::FailClosed("close requires its actor identity result".into())
                    })?,
                    value,
                )?;
                self.ctx.i32_type().const_zero()
            }
            ActorOperation::Spawn(_) => self.emit_actor_spawn(actor, &sources, result)?,
            ActorOperation::SelfHandle(_) => {
                let result = result.ok_or_else(|| {
                    CodegenError::FailClosed("`self` requires its actor handle result".into())
                })?;
                let target = TargetData::create(&self.module.target.data_layout);
                let size_ty = self.ctx.ptr_sized_int_type(&target, None);
                let token = coro::external(
                    self.llvm,
                    "hew_actor_self_token",
                    size_ty.fn_type(&[], false),
                )?;
                let value = self
                    .builder
                    .build_call(token, &[], "self.token")
                    .llvm_ctx("read the running actor's own handle")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| CodegenError::FailClosed("self handle returned void".into()))?;
                self.store(result, value)?;
                self.ctx.i32_type().const_zero()
            }
            ActorOperation::AwaitClosed(_)
            | ActorOperation::SupervisorSpawn(_)
            | ActorOperation::SupervisorChild { .. }
            | ActorOperation::SupervisorAwaitRestart { .. }
            | ActorOperation::SupervisorAwaitClosed(_)
            | ActorOperation::SupervisorRoleAwaitClosed { .. }
            | ActorOperation::SupervisorStop(_) => unreachable!("emitted above"),
            ActorOperation::StreamStart { message, .. } => {
                self.emit_actor_stream_start(actor, message, &sources, unwind)?
            }
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
                self.emit_actor_submit(
                    actor,
                    policy,
                    &message_ty,
                    &result_ty,
                    *source,
                    result,
                    unwind,
                )?;
                self.ctx.i32_type().const_zero()
            }
        };
        for source in sources {
            self.clear_owned(source)?;
        }
        self.builder
            .build_store(self.active_status, status)
            .llvm_ctx("record actor boundary status")?;
        self.emit_call_outcome(status, result, Some(normal), unwind)
    }

    #[allow(
        clippy::too_many_lines,
        reason = "one native observation boundary owns fault routing and the checked Result construction"
    )]
    fn emit_local_observation(
        &self,
        kind: hew_mir::physical::LocalObservationKind,
        transfers: &[ArgumentTransfer],
        result: Option<StorageId>,
        normal: &PhysicalEdge,
        unwind: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        use hew_mir::physical::LocalObservationKind;
        let [ArgumentTransfer::Move(target)] = transfers else {
            return Err(CodegenError::FailClosed(
                "local observation lacks its value target".into(),
            ));
        };
        let target_value = self.load(*target, "observation.target")?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        // One scratch seat per invocation, even when an O0 loop registers
        // repeatedly. An alloca in the loop body would accumulate stack space.
        let id = if kind == LocalObservationKind::Monitor {
            let scratch = self.ctx.create_builder();
            let entry = self.value.get_first_basic_block().ok_or_else(|| {
                CodegenError::FailClosed("monitor boundary lacks its callable entry".into())
            })?;
            if let Some(first) = entry.get_first_instruction() {
                scratch.position_before(&first);
            } else {
                scratch.position_at_end(entry);
            }
            scratch
                .build_alloca(self.ctx.i64_type(), "monitor.id")
                .llvm_ctx("allocate monitor identity output")?
        } else {
            ptr.const_null()
        };
        let (symbol, return_type) = match kind {
            LocalObservationKind::Link => ("hew_native_actor_link", Some(self.ctx.i32_type())),
            LocalObservationKind::Monitor => {
                ("hew_native_actor_monitor", Some(self.ctx.i32_type()))
            }
            LocalObservationKind::Unlink => ("hew_native_actor_unlink", None),
            LocalObservationKind::Demonitor => ("hew_actor_demonitor", None),
        };
        let mut types = vec![target_value.get_type().into()];
        let mut args = vec![target_value.into()];
        if kind == LocalObservationKind::Monitor {
            types.push(ptr.into());
            args.push(id.into());
        }
        let function_type = return_type.map_or_else(
            || self.ctx.void_type().fn_type(&types, false),
            |ty| ty.fn_type(&types, false),
        );
        let function = coro::external(self.llvm, symbol, function_type)?;
        if return_type.is_none() {
            self.builder
                .build_call(function, &args, "")
                .llvm_ctx("remove local observation")?;
            self.clear_owned(*target)?;
            return self.emit_result_edge(None, normal);
        }
        let status =
            call_value(&self.builder, function, &args, "observation.status")?.into_int_value();
        self.clear_owned(*target)?;
        let typed = self.ctx.append_basic_block(self.value, "observation.typed");
        let fault = self.ctx.append_basic_block(self.value, "observation.fault");
        let failed = self
            .builder
            .build_int_compare(
                inkwell::IntPredicate::SLT,
                status,
                self.ctx.i32_type().const_zero(),
                "observation.faulted",
            )
            .llvm_ctx("separate logical faults from typed refusal")?;
        self.builder
            .build_conditional_branch(failed, fault, typed)
            .llvm_ctx("route observation fault")?;
        self.builder.position_at_end(fault);
        let code = self
            .builder
            .build_int_neg(status, "observation.fault.code")
            .llvm_ctx("decode observation fault")?;
        self.initialize_active_fault_value(code)?;
        if let Some(unwind) = unwind {
            self.emit_edge(unwind)?;
        } else {
            self.emit_propagate_fault()?;
        }
        self.builder.position_at_end(typed);
        let result = result.ok_or_else(|| {
            CodegenError::FailClosed("observation lacks its checked result".into())
        })?;
        let result_ty = &self.storage(result)?.ty;
        let glue = self
            .module
            .variant_glue
            .iter()
            .find(|glue| glue.ty == *result_ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("observation result lacks its variant recipe".into())
            })?;
        let error_ty = &glue.variants[1].fields[0].ty;
        let success = self
            .ctx
            .append_basic_block(self.value, "observation.success");
        let failure = self
            .ctx
            .append_basic_block(self.value, "observation.failure");
        let complete = self
            .ctx
            .append_basic_block(self.value, "observation.complete");
        let ok = self
            .builder
            .build_int_compare(
                inkwell::IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_zero(),
                "observation.ok",
            )
            .llvm_ctx("classify local observation")?;
        self.builder
            .build_conditional_branch(ok, success, failure)
            .llvm_ctx("select observation result")?;
        self.builder.position_at_end(success);
        let fields = if kind == LocalObservationKind::Monitor {
            let id = self
                .builder
                .build_load(self.ctx.i64_type(), id, "monitor.identity")
                .llvm_ctx("take monitor identity")?;
            vec![self.ask_record(&glue.variants[0].fields[0].ty, &[id])?]
        } else {
            let layout = self
                .module
                .target
                .layout(&ResolvedTy::Unit)
                .ok_or_else(|| {
                    CodegenError::FailClosed("link success lacks its unit layout".into())
                })?;
            vec![llvm_type(self.ctx, &layout.repr)?.const_zero()]
        };
        self.write_variant_value(self.slots[result.0 as usize], 0, &fields, glue.id)?;
        self.builder
            .build_unconditional_branch(complete)
            .llvm_ctx("complete successful observation")?;
        self.builder.position_at_end(failure);
        let tag = self
            .builder
            .build_int_sub(
                status,
                self.ctx.i32_type().const_int(1, false),
                "observation.error.tag",
            )
            .llvm_ctx("decode LinkError status")?;
        let error = self.actor_unit_variant(error_ty, tag)?;
        self.write_variant_value(self.slots[result.0 as usize], 1, &[error], glue.id)?;
        self.builder
            .build_unconditional_branch(complete)
            .llvm_ctx("complete refused observation")?;
        self.builder.position_at_end(complete);
        self.emit_result_edge(Some(result), normal)
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
        // Init and then `#[on(start)]` run before publication; a fault in
        // either destroys the unpublished state and fails the spawn.
        let bodies: Vec<_> = actor.init.iter().chain(&actor.start).copied().collect();
        let init_failure = if bodies.is_empty() {
            None
        } else {
            let failed = self.ctx.append_basic_block(self.value, "actor.init.failed");
            let mut failures = Vec::new();
            for body in bodies {
                let callable = callable(self.module, body)?;
                let mut arguments: Vec<BasicMetadataValueEnum<'ctx>> = vec![state.into()];
                if Some(body) == actor.init {
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
                }
                arguments.push(self.active_fault.into());
                let status = self
                    .builder
                    .build_call(self.functions[&body], &arguments, "actor.init.status")
                    .llvm_ctx("initialize actor before publication")?
                    .try_as_basic_value()
                    .basic()
                    .unwrap()
                    .into_int_value();
                let initialized = self.ctx.append_basic_block(self.value, "actor.initialized");
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
                failures.push((status, self.builder.get_insert_block().unwrap()));
                self.builder.position_at_end(initialized);
            }
            let initialized = self.builder.get_insert_block().unwrap();
            self.builder.position_at_end(failed);
            let status = self
                .builder
                .build_phi(self.ctx.i32_type(), "actor.init.failure")
                .llvm_ctx("join initialization failures")?;
            for (value, block) in &failures {
                status.add_incoming(&[(value, *block)]);
            }
            let status = status.as_basic_value().into_int_value();
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
        };
        let target = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let periodic_ty = self.ctx.struct_type(
            &[self.ctx.i32_type().into(), self.ctx.i64_type().into()],
            false,
        );
        let periodic: Vec<_> = actor
            .handlers
            .iter()
            .filter_map(|handler| handler.every_ns.map(|ns| (handler, ns)))
            .map(|(handler, ns)| {
                let interval = u64::try_from(ns)
                    .ok()
                    .filter(|ns| *ns >= 1_000_000)
                    .ok_or_else(|| {
                        CodegenError::FailClosed("invalid checked periodic interval".into())
                    })?
                    / 1_000_000;
                Ok(periodic_ty.const_named_struct(&[
                    self.ctx
                        .i32_type()
                        .const_int(u64::from(handler.message_id), false)
                        .into(),
                    self.ctx.i64_type().const_int(interval, false).into(),
                ]))
            })
            .collect::<CodegenResult<Vec<_>>>()?;
        let periodic_count = u32::try_from(periodic.len())
            .map_err(|_| CodegenError::FailClosed("too many periodic handlers".into()))?;
        let periodic_table = if periodic.is_empty() {
            ptr.const_null()
        } else {
            let table = self.llvm.add_global(
                periodic_ty.array_type(periodic_count),
                None,
                &symbol(actor.id, "periodic"),
            );
            table.set_linkage(Linkage::Internal);
            table.set_constant(true);
            table.set_initializer(&periodic_ty.const_array(&periodic));
            table.as_pointer_value()
        };
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
                    ptr.into(),
                    self.ctx.i32_type().into(),
                    self.ctx.i32_type().into(),
                    size_ty.into(),
                    ptr.into(),
                    size_ty.into(),
                    ptr.into(),
                    ptr.into(),
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
        let terminate = if actor.stop.is_empty() {
            ptr.const_null()
        } else {
            callback("terminate")?
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
                    terminate.into(),
                    self.ctx
                        .i32_type()
                        .const_int(u64::from(actor.mailbox_capacity.unwrap_or(0)), false)
                        .into(),
                    self.ctx.i32_type().const_int(overflow, false).into(),
                    size_ty
                        .const_int(actor.max_heap_bytes.unwrap_or(0), false)
                        .into(),
                    periodic_table.into(),
                    size_ty.const_int(u64::from(periodic_count), false).into(),
                    if actor.exit.is_some() || actor.down.is_some() {
                        callback("sys_dispatch")?
                    } else {
                        ptr.const_null()
                    }
                    .into(),
                    if actor.crash.is_some() {
                        callback("on_crash")?
                    } else {
                        ptr.const_null()
                    }
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

    /// The request waits for mailbox capacity like a `.Wait` submission. A
    /// refused request destroys the payload, closing the consumer's sink, and
    /// faults the caller.
    fn emit_actor_stream_start(
        &self,
        actor: &SemActor,
        message: u32,
        sources: &[StorageId],
        unwind: Option<&PhysicalEdge>,
    ) -> CodegenResult<IntValue<'ctx>> {
        let [target, payload] = sources else {
            return Err(CodegenError::FailClosed(
                "stream start requires its target and request payload".into(),
            ));
        };
        let handler = actor
            .handlers
            .iter()
            .find(|handler| handler.message_id == message)
            .ok_or_else(|| CodegenError::FailClosed("stream start lacks its producer".into()))?;
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
        let wrapper = call_value(
            &self.builder,
            allocate,
            &[size_ty.const_int(size, false).into()],
            "stream.start.allocate",
        )?
        .into_pointer_value();
        let allocated = self
            .ctx
            .append_basic_block(self.value, "stream.start.allocated");
        let refused = self
            .ctx
            .append_basic_block(self.value, "stream.start.refused");
        let done = self.ctx.append_basic_block(self.value, "stream.start.done");
        let missing = self
            .builder
            .build_is_null(wrapper, "stream.start.no_memory")
            .llvm_ctx("check request allocation")?;
        self.builder
            .build_conditional_branch(missing, refused, allocated)
            .llvm_ctx("branch on request allocation")?;
        self.builder.position_at_end(allocated);
        self.builder
            .build_store(wrapper, self.ctx.i8_type().const_int(1, false))
            .llvm_ctx("initialize request ownership")?;
        let fields = self
            .load(*payload, "stream.start.payload")?
            .into_struct_value();
        for index in 0..handler.params.len() {
            let index = u32::try_from(index)
                .map_err(|_| CodegenError::FailClosed("request index exceeds u32".into()))?;
            let slot = self
                .builder
                .build_struct_gep(wrapper_ty, wrapper, index + 1, "stream.start.field")
                .llvm_ctx("address request field")?;
            let field = self
                .builder
                .build_extract_value(fields, index, "stream.start.value")
                .llvm_ctx("read request field")?;
            self.builder
                .build_store(slot, field)
                .llvm_ctx("transfer field into unpublished request")?;
        }
        let drop = self
            .llvm
            .get_function(&message_symbol(actor.id, message))
            .ok_or_else(|| {
                CodegenError::FailClosed("stream request lacks its payload destructor".into())
            })?;
        let request = [
            self.load(*target, "stream.start.target")?.into(),
            self.ctx
                .i32_type()
                .const_int(u64::from(message), false)
                .into(),
            wrapper.into(),
            size_ty.const_int(size, false).into(),
            drop.as_global_value().as_pointer_value().into(),
        ];
        let status = self.emit_actor_send_wait(&request, *payload, unwind)?;
        let accepted = self
            .ctx
            .append_basic_block(self.value, "stream.start.accepted");
        let admitted = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_zero(),
                "stream.start.admitted",
            )
            .llvm_ctx("check request admission")?;
        self.builder
            .build_conditional_branch(admitted, accepted, refused)
            .llvm_ctx("select request admission")?;
        self.builder.position_at_end(accepted);
        self.clear_owned(*payload)?;
        self.builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish accepted stream start")?;
        self.builder.position_at_end(refused);
        self.discard_pending_message(*payload)?;
        self.initialize_active_fault(HEW_TRAP_ACTOR_SEND_FAILED)?;
        self.builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish refused stream start")?;
        self.builder.position_at_end(done);
        let outcome = self
            .builder
            .build_phi(self.ctx.i32_type(), "stream.start.status")
            .llvm_ctx("join stream start outcome")?;
        outcome.add_incoming(&[
            (&self.ctx.i32_type().const_zero(), accepted),
            (&self.ctx.i32_type().const_int(1, false), refused),
        ]);
        Ok(outcome.as_basic_value().into_int_value())
    }

    #[expect(
        clippy::too_many_arguments,
        reason = "typed submission operands and cleanup edge"
    )]
    fn emit_actor_submit(
        &self,
        actor: &SemActor,
        policy: hew_types::actor_delivery::SendPolicy,
        message_ty: &ResolvedTy,
        result_ty: &ResolvedTy,
        source: StorageId,
        destination: StorageId,
        unwind: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        use hew_types::actor_delivery::SendPolicy;
        if policy == SendPolicy::ReplaceLatest {
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
            .find(|handler| handler.owes_no_reply() && handler.params == *params)
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
        // A role addresses its actor through the supervisor: resolve the
        // current incarnation here, at the send.
        let role = args[0]
            .is_builtin(hew_types::BuiltinType::ChildRef)
            .then(|| self.resolve_role(target.into_struct_value()))
            .transpose()?;
        let target = role.map_or(target, |(handle, _)| handle.into());
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
        let submitted = self.ctx.append_basic_block(self.value, "submission.result");
        // A role with no live occupant never allocates a wrapper: the message
        // stays with the sender and the refusal names why, spent or restarting.
        let vacant = role
            .map(|(_, tag)| {
                let live = self
                    .ctx
                    .append_basic_block(self.value, "submission.role.live");
                let vacant = self
                    .ctx
                    .append_basic_block(self.value, "submission.role.vacant");
                let occupied = self
                    .builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        tag,
                        self.ctx.i32_type().const_zero(),
                        "submission.role.occupied",
                    )
                    .llvm_ctx("classify the role's occupant")?;
                self.builder
                    .build_conditional_branch(occupied, live, vacant)
                    .llvm_ctx("send only to an occupied role")?;
                self.builder.position_at_end(vacant);
                let spent = self
                    .builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        tag,
                        self.ctx.i32_type().const_int(2, false),
                        "submission.role.spent",
                    )
                    .llvm_ctx("separate a spent role from a restarting one")?;
                let status = self
                    .builder
                    .build_select(
                        spent,
                        self.ctx.i32_type().const_int(5, false),
                        self.ctx.i32_type().const_int(2, false),
                        "submission.role.status",
                    )
                    .llvm_ctx("report why the role took no message")?
                    .into_int_value();
                self.builder
                    .build_unconditional_branch(submitted)
                    .llvm_ctx("return the vacant role refusal")?;
                self.builder.position_at_end(live);
                CodegenResult::Ok((status, vacant))
            })
            .transpose()?;
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
        let request = [
            target.into(),
            member.into(),
            wrapper.into(),
            size_ty.const_int(size, false).into(),
            drop.as_global_value().as_pointer_value().into(),
        ];
        let status = if policy == SendPolicy::Wait {
            self.emit_actor_send_wait(&request, source, unwind)?
        } else {
            let mut args = request.to_vec();
            args.push(
                self.ctx
                    .i32_type()
                    .const_int(
                        if policy == SendPolicy::DropNewest {
                            2
                        } else {
                            0
                        },
                        false,
                    )
                    .into(),
            );
            call_value(&self.builder, submit, &args, "submission.status")?.into_int_value()
        };
        let admission_block = self.builder.get_insert_block().unwrap();
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
            (&status, admission_block),
        ]);
        if let Some((status, block)) = vacant {
            outcome.add_incoming(&[(&status, block)]);
        }
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
        let spent = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_int(5, false),
                "submission.spent",
            )
            .llvm_ctx("classify a spent supervised role")?;
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
                spent,
                self.ctx.i8_type().const_int(11, false),
                reason.into_int_value(),
                "submission.reason.role",
            )
            .llvm_ctx("name a spent supervised role")?;
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

    /// Construct the field-less variant a runtime status tag names. The payload
    /// seat stays zeroed: the enum's drop glue dispatches on the tag, so a
    /// variant that declares no fields never reads it. Enums whose other
    /// variants do carry payloads (`ActorError.Rejected`, `ActorError.Failed`)
    /// are built at their own construction sites, never from a status tag.
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
            .ok_or_else(|| {
                CodegenError::FailClosed("delivery status requires its exact variant recipe".into())
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
