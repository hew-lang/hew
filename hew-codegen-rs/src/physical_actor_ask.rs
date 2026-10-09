//! Exact request transfer and typed reply materialization around native readiness.

use super::*;

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    #[allow(clippy::too_many_arguments, reason = "exact actor suspension contract")]
    pub(in crate::physical) fn emit_actor_ask(
        &self,
        actor: ActorId,
        message: u32,
        policy: hew_types::actor_delivery::SendPolicy,
        sealed: bool,
        args: &[ArgumentTransfer],
        result: StorageId,
        normal: &PhysicalEdge,
        cancel: &PhysicalEdge,
        unwind: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("ask requires a resumable invocation".into())
        })?;
        let operation = self.emit_actor_call_start(actor, message, policy, sealed, args)?;
        let ArgumentTransfer::Borrow(target) = args[0] else {
            return Err(CodegenError::FailClosed(
                "ask must borrow its target".into(),
            ));
        };
        let wait_edge =
            self.new_actor_wait_edge(self.load_actor_target(target, "ask.wait.target")?.into(), 0)?;
        frame.carry(self.ctx, &self.builder, operation, "ask.operation.slot")?;
        frame.carry(self.ctx, &self.builder, wait_edge, "ask.wait.edge.slot")?;
        let poll = self.ctx.append_basic_block(self.value, "ask.poll");
        let inspect = self.ctx.append_basic_block(self.value, "ask.inspect");
        let pending = self.ctx.append_basic_block(self.value, "ask.pending");
        let completed = self.ctx.append_basic_block(self.value, "ask.completed");
        let cancelled = self.ctx.append_basic_block(self.value, "ask.cancelled");
        let destroyed = self.ctx.append_basic_block(self.value, "ask.destroyed");
        let failed = self.ctx.append_basic_block(self.value, "ask.failed");
        let cycle = self.ctx.append_basic_block(self.value, "ask.cycle");
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("poll completion")?;
        self.builder.position_at_end(poll);
        self.free_handle("hew_actor_wait_edge_prepare", wait_edge)?;
        let cancelling = self.state_value("hew_coro_state_is_cancelled", frame.state)?;
        let cancelling = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                cancelling,
                self.ctx.i32_type().const_zero(),
                "ask.cancel.requested",
            )
            .llvm_ctx("inspect completion cancellation")?;
        self.builder
            .build_conditional_branch(cancelling, cancelled, inspect)
            .llvm_ctx("select completion cancellation")?;
        self.builder.position_at_end(inspect);
        let status = self.state_value("hew_actor_call_poll", operation)?;
        self.builder
            .build_switch(
                status,
                completed,
                &[
                    (self.ctx.i32_type().const_all_ones(), pending),
                    (self.ctx.i32_type().const_int((-2_i64) as u64, true), failed),
                ],
            )
            .llvm_ctx("inspect completion readiness")?;
        self.builder.position_at_end(pending);
        self.check_actor_wait_cycle(wait_edge, cycle)?;
        frame.suspend(self.ctx, &self.builder, poll, destroyed)?;
        self.builder.position_at_end(destroyed);
        self.builder
            .build_store(frame.destroying, self.ctx.bool_type().const_int(1, false))
            .llvm_ctx("mark destroyed completion frame")?;
        self.builder
            .build_unconditional_branch(cancelled)
            .llvm_ctx("abandon destroyed completion")?;
        // Every exit records its fault, then converges on one release: the
        // wait edge and the call are freed once, and the exit code selects the
        // MIR edge.
        let mut join = self.exit_join("ask.release");
        self.builder.position_at_end(completed);
        self.emit_actor_call_take(operation, actor, message, policy, target, result)?;
        self.leave(&mut join, suspend::EXIT_NORMAL)?;
        self.builder.position_at_end(cancelled);
        self.initialize_cancellation_fault()?;
        self.leave(&mut join, suspend::EXIT_CANCEL)?;
        self.builder.position_at_end(cycle);
        self.initialize_actor_cycle_fault(wait_edge)?;
        self.leave(&mut join, suspend::EXIT_UNWIND)?;
        self.builder.position_at_end(failed);
        self.initialize_active_fault(HEW_TRAP_USER_PANIC)?;
        self.leave(&mut join, suspend::EXIT_UNWIND)?;
        let selected = self.enter_join(&join)?;
        self.free_handle("hew_actor_wait_edge_free", wait_edge)?;
        self.release_handle(release::Handle::ActorCall, operation)?;
        let ([normal_block, cancel_block], unwind_block) = self.dispatch_exits(
            selected,
            [
                (suspend::EXIT_NORMAL, "ask.normal"),
                (suspend::EXIT_CANCEL, "ask.cancel"),
            ],
            "ask.unwind",
        )?;
        self.builder.position_at_end(normal_block);
        self.emit_result_edge(Some(result), normal)?;
        self.builder.position_at_end(cancel_block);
        self.emit_edge(cancel)?;
        self.builder.position_at_end(unwind_block);
        self.emit_edge(unwind)
    }

    /// Transfer the checked request into the shared runtime operation. Starting
    /// never parks: later select operands may still be evaluated or fail.
    #[allow(
        clippy::too_many_arguments,
        clippy::too_many_lines,
        reason = "one exact request allocation and transfer boundary"
    )]
    pub(super) fn emit_actor_call_start(
        &self,
        actor: ActorId,
        message: u32,
        policy: hew_types::actor_delivery::SendPolicy,
        sealed: bool,
        args: &[ArgumentTransfer],
    ) -> CodegenResult<PointerValue<'ctx>> {
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("completion start requires an invocation state".into())
        })?;
        let actor = self.module.actors.get(actor.0 as usize).ok_or_else(|| {
            CodegenError::FailClosed("completion lacks its actor descriptor".into())
        })?;
        let handler = actor
            .handlers
            .iter()
            .find(|handler| handler.message_id == message)
            .ok_or_else(|| {
                CodegenError::FailClosed("completion lacks its receive member".into())
            })?;
        let sources = args
            .iter()
            .enumerate()
            .map(|(index, argument)| match (index, argument) {
                (0, ArgumentTransfer::Borrow(source)) | (_, ArgumentTransfer::Move(source)) => {
                    Ok(*source)
                }
                _ => Err(CodegenError::FailClosed(
                    "completion changes request ownership".into(),
                )),
            })
            .collect::<CodegenResult<Vec<_>>>()?;
        let target = TargetData::create(&self.module.target.data_layout);
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let wrapper_ty = message_type(self.module, self.ctx, handler)?;
        let size = target.get_abi_size(&wrapper_ty);
        let wrapper = if sealed {
            self.load(sources[1], "ask.sealed.request")?
                .into_pointer_value()
        } else {
            let allocate = coro::external(
                self.llvm,
                "hew_actor_payload_try_alloc",
                ptr.fn_type(&[size_ty.into()], false),
            )?;
            let wrapper = call_value(
                &self.builder,
                allocate,
                &[size_ty.const_int(size, false).into()],
                "ask.request",
            )?
            .into_pointer_value();
            frame.carry(self.ctx, &self.builder, wrapper, "ask.request.slot")?;
            let failed = self
                .ctx
                .append_basic_block(self.value, "ask.allocation.failed");
            let populate = self.ctx.append_basic_block(self.value, "ask.populate");
            let start = self.ctx.append_basic_block(self.value, "ask.start");
            let missing = self
                .builder
                .build_is_null(wrapper, "ask.missing.request")
                .llvm_ctx("check request allocation")?;
            self.builder
                .build_conditional_branch(missing, failed, populate)
                .llvm_ctx("retain fields until request allocation")?;
            self.builder.position_at_end(failed);
            for source in sources.iter().skip(1) {
                if let Some(action) = self
                    .module
                    .actor_recipes
                    .get(&self.storage(*source)?.ty)
                    .and_then(|recipe| recipe.destroy)
                {
                    self.destroy_owned_operand(*source, action)?;
                } else {
                    self.clear_owned(*source)?;
                }
            }
            self.builder
                .build_unconditional_branch(start)
                .llvm_ctx("start failed allocation outcome")?;
            self.builder.position_at_end(populate);
            self.builder
                .build_store(wrapper, self.ctx.i8_type().const_int(1, false))
                .llvm_ctx("initialize request ownership")?;
            for (index, source) in sources.iter().skip(1).enumerate() {
                let field = self
                    .builder
                    .build_struct_gep(wrapper_ty, wrapper, (index + 1) as u32, "ask.request.field")
                    .llvm_ctx("address request field")?;
                self.builder
                    .build_store(field, self.load(*source, "ask.argument")?)
                    .llvm_ctx("transfer request field")?;
            }
            self.builder
                .build_unconditional_branch(start)
                .llvm_ctx("start populated request")?;
            self.builder.position_at_end(start);
            wrapper
        };
        let wake = coro::external(
            self.llvm,
            "hew_coro_state_waker",
            ptr.fn_type(&[ptr.into()], false),
        )?;
        let waker = call_value(&self.builder, wake, &[frame.state.into()], "ask.waker")?;
        let drop_reply = self
            .llvm
            .get_function(&reply_symbol(actor.id, message))
            .map_or(ptr.const_null(), |function| {
                function.as_global_value().as_pointer_value()
            });
        let reply_size = callable(self.module, handler.callable)?
            .return_layout
            .as_ref()
            .map_or(0, |layout| layout.size);
        let mut types: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> =
            vec![size_ty.into(), self.ctx.i32_type().into(), ptr.into()];
        let mut arguments: Vec<inkwell::values::BasicMetadataValueEnum<'ctx>> = vec![
            self.load_actor_target(sources[0], "ask.target")?.into(),
            self.ctx
                .i32_type()
                .const_int(u64::from(message), false)
                .into(),
            wrapper.into(),
        ];
        if !sealed {
            let drop_request = self
                .llvm
                .get_function(&message_symbol(actor.id, message))
                .ok_or_else(|| CodegenError::FailClosed("request lacks its destructor".into()))?;
            types.extend::<[inkwell::types::BasicMetadataTypeEnum<'ctx>; 2]>([
                size_ty.into(),
                ptr.into(),
            ]);
            arguments.extend::<[inkwell::values::BasicMetadataValueEnum<'ctx>; 2]>([
                size_ty.const_int(size, false).into(),
                drop_request.as_global_value().as_pointer_value().into(),
            ]);
        }
        types.extend::<[inkwell::types::BasicMetadataTypeEnum<'ctx>; 6]>([
            size_ty.into(),
            ptr.into(),
            ptr.into(),
            self.ctx.i64_type().into(),
            self.ctx.i32_type().into(),
            self.ctx.i32_type().into(),
        ]);
        arguments.extend::<[inkwell::values::BasicMetadataValueEnum<'ctx>; 6]>([
            size_ty.const_int(reply_size, false).into(),
            drop_reply.into(),
            waker.into(),
            // A source ask carries no deadline; the runtime timer serves the
            // remote-local route only.
            self.ctx.i64_type().const_zero().into(),
            self.ctx.i32_type().const_zero().into(),
            self.ctx
                .i32_type()
                .const_int(
                    u64::from(policy == hew_types::actor_delivery::SendPolicy::Reject),
                    false,
                )
                .into(),
        ]);
        if !sealed {
            types.push(ptr.into());
            arguments.push(message_release(self.llvm, self.ctx, actor.id, message).into());
        }
        types.push(ptr.into());
        arguments.push(
            actor_value_release(self.ctx, self.llvm, self.module, &handler.return_ty)?.into(),
        );
        let start = coro::external(
            self.llvm,
            if sealed {
                "hew_actor_call_resume"
            } else {
                "hew_actor_call_new"
            },
            ptr.fn_type(&types, false),
        )?;
        let operation =
            call_value(&self.builder, start, &arguments, "ask.operation")?.into_pointer_value();
        for source in sources.iter().skip(1) {
            self.clear_owned(*source)?;
        }
        Ok(operation)
    }

    /// Materialize the selected value using the same reply and rejection recipes
    /// as an ordinary call. The operation releases only what was not taken; the
    /// caller releases the drained handle.
    #[allow(
        clippy::too_many_arguments,
        reason = "exact selected completion protocol"
    )]
    pub(super) fn emit_actor_call_take(
        &self,
        operation: PointerValue<'ctx>,
        actor: ActorId,
        message: u32,
        policy: hew_types::actor_delivery::SendPolicy,
        target: StorageId,
        result: StorageId,
    ) -> CodegenResult<()> {
        let handler = self
            .module
            .actors
            .get(actor.0 as usize)
            .and_then(|actor| {
                actor
                    .handlers
                    .iter()
                    .find(|handler| handler.message_id == message)
            })
            .ok_or_else(|| CodegenError::FailClosed("selected call lacks its protocol".into()))?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let entry = self.ctx.create_builder();
        let block = self.value.get_first_basic_block().unwrap();
        if let Some(first) = block.get_first_instruction() {
            entry.position_before(&first);
        } else {
            entry.position_at_end(block);
        }
        let reply = callable(self.module, handler.callable)?
            .return_layout
            .as_ref()
            .map(|layout| {
                entry
                    .build_alloca(llvm_type(self.ctx, &layout.repr)?, "ask.reply")
                    .llvm_ctx("allocate selected reply slot")
            })
            .transpose()?;
        let request = entry
            .build_alloca(ptr, "ask.rejected.request")
            .llvm_ctx("allocate rejected request slot")?;
        let take = coro::external(
            self.llvm,
            "hew_actor_call_take",
            self.ctx.i32_type().fn_type(&[ptr.into(); 3], false),
        )?;
        let status = call_value(
            &self.builder,
            take,
            &[
                operation.into(),
                reply.unwrap_or(ptr.const_null()).into(),
                request.into(),
            ],
            "ask.outcome",
        )?
        .into_int_value();
        let done = self.ctx.append_basic_block(self.value, "ask.taken");
        if policy == hew_types::actor_delivery::SendPolicy::Reject {
            let rejected = self.ctx.append_basic_block(self.value, "ask.rejected");
            let replied = self.ctx.append_basic_block(self.value, "ask.replied");
            let full = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    status,
                    self.ctx.i32_type().const_int(
                        hew_runtime::internal::types::AskError::MailboxFull as u64,
                        false,
                    ),
                    "ask.refused.full",
                )
                .llvm_ctx("classify full-mailbox rejection")?;
            let shutting_down = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    status,
                    self.ctx.i32_type().const_int(
                        hew_runtime::internal::types::AskError::LocalShutdown as u64,
                        false,
                    ),
                    "ask.refused.shutdown",
                )
                .llvm_ctx("classify shutdown rejection")?;
            let refused = self
                .builder
                .build_or(full, shutting_down, "ask.refused")
                .llvm_ctx("classify request rejection")?;
            self.builder
                .build_conditional_branch(refused, rejected, replied)
                .llvm_ctx("select owned rejection")?;
            self.builder.position_at_end(rejected);
            let request = self
                .builder
                .build_load(ptr, request, "ask.rejected.envelope")
                .llvm_ctx("take rejected envelope")?
                .into_pointer_value();
            self.emit_ask_refused(result, target, message, request, shutting_down)?;
            self.builder
                .build_unconditional_branch(done)
                .llvm_ctx("finish rejected request")?;
            self.builder.position_at_end(replied);
        }
        self.emit_ask_result(result, status, reply, handler)?;
        self.builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish selected completion")?;
        self.builder.position_at_end(done);
        Ok(())
    }

    /// A `policy(target, on_full: .Reject)` call refused admission, because
    /// the destination mailbox is full or the runtime is shutting down:
    /// nothing was accepted, so `Rejected` returns the refusal reason
    /// (`Full` or `LocalShutdown`) and the original owned request. These are
    /// the only refusals a completion call reports; every other outcome means
    /// the request was accepted or its fate is unknown.
    fn emit_ask_refused(
        &self,
        result: StorageId,
        target: StorageId,
        message: u32,
        request: PointerValue<'ctx>,
        shutting_down: IntValue<'ctx>,
    ) -> CodegenResult<()> {
        let glue = self
            .module
            .variant_glue
            .iter()
            .find(|glue| glue.ty == self.storage(result).unwrap().ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("refused call lacks its exact variant recipe".into())
            })?;
        let error_ty = &glue.variants[1].fields[0].ty;
        let error_glue = self
            .module
            .variant_glue
            .iter()
            .find(|candidate| candidate.ty == *error_ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("the call envelope lacks its variant recipe".into())
            })?;
        let failure_ty = error_glue
            .variants
            .first()
            .and_then(|rejected| rejected.fields.first())
            .map(|field| field.ty.clone())
            .ok_or_else(|| {
                CodegenError::FailClosed("`Rejected` lacks its refusal reason seat".into())
            })?;
        let ResolvedTy::Named { args, .. } = &failure_ty else {
            return Err(CodegenError::FailClosed(
                "rejection lacks its SendFailure type".into(),
            ));
        };
        let message_ty = &args[0];
        let ResolvedTy::Named { args, .. } = message_ty else {
            return Err(CodegenError::FailClosed(
                "rejection lacks its Message type".into(),
            ));
        };
        let payload_ty = &args[1];
        let payload = self.ask_record(payload_ty, &[request.into()])?;
        let action = self
            .module
            .actor_recipes
            .get(&self.storage(target)?.ty)
            .and_then(|recipe| recipe.clone)
            .ok_or_else(|| {
                CodegenError::FailClosed("returned request lacks its retained target recipe".into())
            })?;
        let target = self.clone_value(target, action)?;
        let message = self.ask_record(
            message_ty,
            &[
                target,
                self.ctx
                    .i32_type()
                    .const_int(u64::from(message), false)
                    .into(),
                payload,
            ],
        )?;
        let reason_ty = ResolvedTy::from_ty(&hew_types::Ty::send_error())
            .map_err(|error| CodegenError::FailClosed(error.to_string()))?;
        let reason_tag = |role| {
            self.module
                .variant_glue
                .iter()
                .find(|glue| glue.ty == reason_ty)
                .and_then(|glue| glue.runtime_tag(role))
                .map(|tag| self.ctx.i32_type().const_int(u64::from(tag), false))
                .ok_or_else(|| {
                    CodegenError::FailClosed("send refusal reason lacks its runtime role".into())
                })
        };
        let full = reason_tag(hew_mir::RuntimeVariantRole::SendErrorFull)?;
        let local_shutdown = reason_tag(hew_mir::RuntimeVariantRole::SendErrorLocalShutdown)?;
        let tag = self
            .builder
            .build_select(shutting_down, local_shutdown, full, "ask.refused.reason")
            .llvm_ctx("select refusal reason")?
            .into_int_value();
        let reason = self.actor_unit_variant(&reason_ty, tag)?;
        let failure = self.ask_record(&failure_ty, &[reason, message])?;
        let object_ty = llvm_type(
            self.ctx,
            &self.value_emitter().variant_layout(error_ty)?.object.repr,
        )?
        .into_struct_type();
        let scratch = self
            .builder
            .build_alloca(object_ty, "ask.refused")
            .llvm_ctx("allocate the refusal envelope")?;
        self.write_variant_value(scratch, 0, &[failure], error_glue.id)?;
        let envelope = self
            .builder
            .build_load(object_ty, scratch, "ask.refused.value")
            .llvm_ctx("take the refusal envelope")?;
        self.write_variant_value(self.slots[result.0 as usize], 1, &[envelope], glue.id)
    }

    pub(super) fn ask_record(
        &self,
        ty: &ResolvedTy,
        fields: &[BasicValueEnum<'ctx>],
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let layout = self.module.target.layout(ty).ok_or_else(|| {
            CodegenError::FailClosed("sealed request record lacks its checked layout".into())
        })?;
        let mut value = llvm_type(self.ctx, &layout.repr)?
            .into_struct_type()
            .const_zero();
        for (index, field) in fields.iter().enumerate() {
            value = self
                .builder
                .build_insert_value(
                    value,
                    *field,
                    u32::try_from(index).map_err(|_| {
                        CodegenError::FailClosed("request field index exceeds u32".into())
                    })?,
                    "ask.request.record",
                )
                .llvm_ctx("assemble sealed request owner")?
                .into_struct_value();
        }
        Ok(value.into())
    }

    /// Store `status` and call the shared `__hew_ask_result_*` thunk, which
    /// writes `Ok(reply)` or `Err(ActorError.<role>)` into the result slot.
    pub(super) fn emit_ask_result(
        &self,
        result: StorageId,
        status: IntValue<'ctx>,
        reply: Option<PointerValue<'ctx>>,
        handler: &SemActorHandler,
    ) -> CodegenResult<()> {
        let glue = self
            .module
            .variant_glue
            .iter()
            .find(|glue| glue.ty == self.storage(result).unwrap().ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("ask result lacks its exact variant recipe".into())
            })?;
        let thunk = ask_result_thunk(
            self.ctx,
            self.llvm,
            self.module,
            glue,
            handler,
            reply.is_some(),
        )?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        self.builder
            .build_call(
                thunk,
                &[
                    self.slots[result.0 as usize].into(),
                    status.into(),
                    reply.unwrap_or_else(|| pointer.const_null()).into(),
                ],
                "",
            )
            .llvm_ctx("materialize the ask result")?;
        Ok(())
    }
}

/// The module-level function that writes one ask outcome: `Ok(reply)` for
/// status zero, otherwise `Err(ActorError.<role>)` selected through a tag
/// table indexed by the runtime status. One exists per result type and handler.
fn ask_result_thunk<'ctx>(
    ctx: &'ctx Context,
    llvm: &Module<'ctx>,
    module: &PhysicalModule,
    glue: &PhysicalVariantGlue,
    handler: &SemActorHandler,
    has_reply: bool,
) -> CodegenResult<FunctionValue<'ctx>> {
    let name = format!(
        "__hew_ask_result_{}_{}_{}",
        glue.id.0,
        handler.callable.0,
        u8::from(has_reply)
    );
    if let Some(function) = llvm.get_function(&name) {
        return Ok(function);
    }
    let pointer = ctx.ptr_type(AddressSpace::default());
    let function = llvm.add_function(
        &name,
        ctx.void_type().fn_type(
            &[pointer.into(), ctx.i32_type().into(), pointer.into()],
            false,
        ),
        Some(Linkage::Internal),
    );
    function.add_attribute(
        inkwell::attributes::AttributeLoc::Function,
        ctx.create_enum_attribute(
            inkwell::attributes::Attribute::get_named_enum_kind_id("noinline"),
            0,
        ),
    );
    let builder = ctx.create_builder();
    builder.position_at_end(ctx.append_basic_block(function, "entry"));
    let values = ValueEmitter {
        module,
        ctx,
        llvm,
        builder: &builder,
        value: function,
        fault_sink: None,
    };
    let out = function.get_nth_param(0).unwrap().into_pointer_value();
    let status = function.get_nth_param(1).unwrap().into_int_value();
    let reply = has_reply.then(|| function.get_nth_param(2).unwrap().into_pointer_value());
    values.write_ask_result(glue, handler, out, status, reply)?;
    Ok(function)
}

impl<'ctx> ValueEmitter<'_, 'ctx> {
    fn write_ask_result(
        &self,
        glue: &PhysicalVariantGlue,
        handler: &SemActorHandler,
        out: PointerValue<'ctx>,
        status: IntValue<'ctx>,
        reply: Option<PointerValue<'ctx>>,
    ) -> CodegenResult<()> {
        let error_ty = &glue.variants[1].fields[0].ty;
        let error = self.ctx.append_basic_block(self.value, "ask.error");
        let done = self.ctx.append_basic_block(self.value, "ask.result");
        // A completion call on a void handler carries no reply payload: the
        // handler's return is the unit reply, so success writes `Ok(())`.
        let completion = reply.is_none() && handler.return_ty == ResolvedTy::Unit;
        if reply.is_some() || completion {
            let success = self.ctx.append_basic_block(self.value, "ask.success");
            let ok = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    status,
                    self.ctx.i32_type().const_zero(),
                    "ask.ok",
                )
                .llvm_ctx("classify the ask outcome")?;
            self.builder
                .build_conditional_branch(ok, success, error)
                .llvm_ctx("materialize the ask outcome")?;
            self.builder.position_at_end(success);
        } else {
            self.builder
                .build_unconditional_branch(error)
                .llvm_ctx("materialize admission error")?;
            self.builder.position_at_end(error);
            self.emit_ask_status_error(status, out, error_ty, glue.id, done)?;
            self.builder.position_at_end(done);
            self.builder
                .build_return(None)
                .llvm_ctx("finish the ask result")?;
            return Ok(());
        }
        if let Some(reply) = reply {
            // A `fails` handler replies with its complete `Result<R, E>`: the
            // call's success arm is `R`, and the handler's own `Err(e)` becomes
            // `ActorError.Failed(e)` here, at the only site that owns both.
            let call_reply_ty = glue.variants[0]
                .fields
                .first()
                .map(|field| field.ty.clone())
                .unwrap_or(ResolvedTy::Unit);
            if handler.return_ty == call_reply_ty {
                let layout = self
                    .module
                    .target
                    .layout(&handler.return_ty)
                    .ok_or_else(|| {
                        CodegenError::FailClosed("reply lacks its target layout".into())
                    })?;
                let value = self
                    .builder
                    .build_load(llvm_type(self.ctx, &layout.repr)?, reply, "ask.reply.value")
                    .llvm_ctx("take typed reply")?;
                self.write_variant_value(out, 0, &[value], glue.id)?;
                self.builder
                    .build_unconditional_branch(done)
                    .llvm_ctx("finish successful reply")?;
            } else {
                self.emit_declared_failure_reply(out, reply, handler, error_ty, glue.id, done)?;
            }
        } else {
            // `Ok(())` still carries the unit payload seat the recipe declares.
            let unit = glue
                .variants
                .first()
                .is_some_and(|case| !case.fields.is_empty())
                .then(|| {
                    let layout = self
                        .module
                        .target
                        .layout(&ResolvedTy::Unit)
                        .ok_or_else(|| {
                            CodegenError::FailClosed("unit reply lacks its target layout".into())
                        })?;
                    Ok::<_, CodegenError>(llvm_type(self.ctx, &layout.repr)?.const_zero())
                })
                .transpose()?;
            self.write_variant_value(out, 0, unit.as_slice(), glue.id)?;
            self.builder
                .build_unconditional_branch(done)
                .llvm_ctx("finish completed call")?;
        }
        self.builder.position_at_end(error);
        self.emit_ask_status_error(status, out, error_ty, glue.id, done)?;
        self.builder.position_at_end(done);
        self.builder
            .build_return(None)
            .llvm_ctx("finish the ask result")?;
        Ok(())
    }

    /// Materialize `Err(ActorError.<role>)` from a runtime ask status. Each
    /// status selects its variant by the runtime's role and the std
    /// declaration's tag for that role, never by position; a status the
    /// runtime does not define aborts.
    fn emit_ask_status_error(
        &self,
        status: IntValue<'ctx>,
        out: PointerValue<'ctx>,
        error_ty: &ResolvedTy,
        glue_id: hew_mir::physical::PhysicalVariantId,
        done: inkwell::basic_block::BasicBlock<'ctx>,
    ) -> CodegenResult<()> {
        let error_glue = self
            .module
            .variant_glue
            .iter()
            .find(|glue| glue.ty == *error_ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("ActorError lacks its variant recipe".into())
            })?;
        let (table, len) = self.ask_error_tags(error_glue)?;
        let i32_ty = self.ctx.i32_type();
        let lookup = self.ctx.append_basic_block(self.value, "ask.error.lookup");
        let known = self.ctx.append_basic_block(self.value, "ask.error.known");
        let unknown = self.ctx.append_basic_block(self.value, "ask.error.unknown");
        let in_range = self
            .builder
            .build_int_compare(
                IntPredicate::ULT,
                status,
                i32_ty.const_int(u64::from(len), false),
                "ask.error.defined",
            )
            .llvm_ctx("bound the ask status")?;
        self.builder
            .build_conditional_branch(in_range, lookup, unknown)
            .llvm_ctx("refuse an undefined ask status")?;
        self.builder.position_at_end(lookup);
        let i8_ty = self.ctx.i8_type();
        let entry = unsafe {
            self.builder
                .build_in_bounds_gep(
                    i8_ty.array_type(len),
                    table,
                    &[i32_ty.const_zero(), status],
                    "ask.error.entry",
                )
                .llvm_ctx("address the ActorError tag")?
        };
        let tag = self
            .builder
            .build_load(i8_ty, entry, "ask.error.role")
            .llvm_ctx("read the ActorError tag")?
            .into_int_value();
        let assigned = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                tag,
                i8_ty.const_int(ASK_TAG_NONE, false),
                "ask.error.assigned",
            )
            .llvm_ctx("test the ActorError role")?;
        self.builder
            .build_conditional_branch(assigned, known, unknown)
            .llvm_ctx("select the ActorError role")?;
        self.builder.position_at_end(known);
        let tag = self
            .builder
            .build_int_z_extend(tag, i32_ty, "ask.error.tag")
            .llvm_ctx("widen the ActorError tag")?;
        let error_value = self.actor_unit_variant(error_ty, tag)?;
        self.write_variant_value(out, 1, &[error_value], glue_id)?;
        self.builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish ask error")?;
        self.builder.position_at_end(unknown);
        let abort = coro::external(self.llvm, "abort", self.ctx.void_type().fn_type(&[], false))?;
        self.builder
            .build_call(abort, &[], "")
            .llvm_ctx("refuse an undefined ask status")?;
        self.builder
            .build_unreachable()
            .llvm_ctx("end an undefined ask status")?;
        Ok(())
    }

    /// The `ActorError` tag for each runtime ask status, indexed by status
    /// code; statuses without a public role hold `ASK_TAG_NONE`. One constant
    /// table per `ActorError` shape.
    fn ask_error_tags(
        &self,
        error_glue: &PhysicalVariantGlue,
    ) -> CodegenResult<(PointerValue<'ctx>, u32)> {
        use hew_runtime::internal::types::AskError;
        let len = AskError::ALL
            .iter()
            .map(|status| *status as u32)
            .max()
            .map_or(0, |max| max + 1);
        let name = format!("__hew_ask_error_tags_{}", error_glue.id.0);
        if let Some(table) = self.llvm.get_global(&name) {
            return Ok((table.as_pointer_value(), len));
        }
        let i8_ty = self.ctx.i8_type();
        let mut tags = vec![i8_ty.const_int(ASK_TAG_NONE, false); len as usize];
        for status in AskError::ALL {
            let Some(role) = status.public_role() else {
                continue;
            };
            let tag = error_glue
                .runtime_tag(actor_error_variant_role(role))
                .ok_or_else(|| {
                    CodegenError::FailClosed(format!("ActorError lacks the {role:?} role"))
                })?;
            if u64::from(tag) >= ASK_TAG_NONE {
                return Err(CodegenError::FailClosed(
                    "ActorError tag exceeds the ask tag table".into(),
                ));
            }
            tags[status as usize] = i8_ty.const_int(u64::from(tag), false);
        }
        let table = self.llvm.add_global(i8_ty.array_type(len), None, &name);
        table.set_initializer(&i8_ty.const_array(&tags));
        table.set_constant(true);
        table.set_linkage(Linkage::Private);
        table.set_unnamed_addr(true);
        Ok((table.as_pointer_value(), len))
    }

    /// Unwrap a `fails` handler's `Result<R, E>` reply into the call envelope:
    /// `Ok(r)` is the call's own `Ok`, and `Err(e)` is `ActorError.Failed(e)`.
    fn emit_declared_failure_reply(
        &self,
        out: PointerValue<'ctx>,
        reply: PointerValue<'ctx>,
        handler: &SemActorHandler,
        error_ty: &ResolvedTy,
        glue_id: hew_mir::physical::PhysicalVariantId,
        done: inkwell::basic_block::BasicBlock<'ctx>,
    ) -> CodegenResult<()> {
        let error_glue = self
            .module
            .variant_glue
            .iter()
            .find(|glue| glue.ty == *error_ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("the call envelope lacks its variant recipe".into())
            })?;
        let wire_layout = self.variant_layout(&handler.return_ty)?;
        let object = self.variant_object_ptr(reply, wire_layout)?;
        let tag = self.load_variant_tag(object, wire_layout)?;
        let replied = self.ctx.append_basic_block(self.value, "ask.reply.ok");
        let failed = self.ctx.append_basic_block(self.value, "ask.reply.failed");
        let is_ok = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                tag,
                tag.get_type().const_zero(),
                "ask.reply.declared",
            )
            .llvm_ctx("classify a declared handler failure")?;
        self.builder
            .build_conditional_branch(is_ok, replied, failed)
            .llvm_ctx("select the declared reply arm")?;
        for (block, wire_variant, call_variant) in [(replied, 0_usize, 0_u32), (failed, 1, 1)] {
            self.builder.position_at_end(block);
            let payload_ty =
                llvm_type(self.ctx, &wire_layout.variants[wire_variant].repr)?.into_struct_type();
            let payload = self
                .builder
                .build_load(
                    payload_ty,
                    self.variant_payload_ptr(object, wire_layout)?,
                    "ask.reply.payload",
                )
                .llvm_ctx("read the declared reply payload")?
                .into_struct_value();
            let field = self
                .builder
                .build_extract_value(payload, 0, "ask.reply.field")
                .llvm_ctx("take the declared reply field")?;
            let value = if call_variant == 0 {
                field
            } else {
                let object_ty = llvm_type(self.ctx, &self.variant_layout(error_ty)?.object.repr)?
                    .into_struct_type();
                let scratch = self
                    .builder
                    .build_alloca(object_ty, "ask.failed")
                    .llvm_ctx("allocate the declared failure envelope")?;
                self.write_variant_value(scratch, 1, &[field], error_glue.id)?;
                self.builder
                    .build_load(object_ty, scratch, "ask.failed.value")
                    .llvm_ctx("take the declared failure envelope")?
            };
            self.write_variant_value(out, call_variant, &[value], glue_id)?;
            self.builder
                .build_unconditional_branch(done)
                .llvm_ctx("finish the declared reply arm")?;
        }
        Ok(())
    }
}

/// An `ActorError` tag table entry for a status that names no variant.
const ASK_TAG_NONE: u64 = 0xFF;

/// The SIR role naming the std `ActorError` variant a runtime role reports.
pub(super) const fn actor_error_variant_role(
    role: hew_runtime::internal::types::ActorErrorRole,
) -> hew_mir::RuntimeVariantRole {
    use hew_mir::RuntimeVariantRole as Variant;
    use hew_runtime::internal::types::ActorErrorRole as Role;
    match role {
        Role::Trapped => Variant::ActorErrorTrapped,
        Role::Dead => Variant::ActorErrorDead,
        Role::TimedOut => Variant::ActorErrorTimedOut,
        Role::NodeNotRunning => Variant::ActorErrorNodeNotRunning,
        Role::RoutingFailed => Variant::ActorErrorRoutingFailed,
        Role::EncodeFailed => Variant::ActorErrorEncodeFailed,
        Role::ConnectionDropped => Variant::ActorErrorConnectionDropped,
        Role::Partition => Variant::ActorErrorPartition,
    }
}

#[cfg(test)]
mod actor_error_role_tests {
    use hew_runtime::internal::types::AskError;

    /// Every runtime ask failure reaches the `ActorError` role of the same
    /// name; SIR then joins that role to the std variant by name, so the tag
    /// follows the declaration rather than a position.
    #[test]
    fn every_ask_failure_selects_its_named_actor_error_role() {
        for status in AskError::ALL {
            let Some(role) = status.public_role() else {
                assert_eq!(status, AskError::None, "only success has no role");
                continue;
            };
            assert_eq!(
                format!("{:?}", super::actor_error_variant_role(role)),
                format!("ActorError{role:?}"),
                "{status:?} drifted from its ActorError role"
            );
        }
    }
}
