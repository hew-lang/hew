//! Exact request transfer and typed reply materialization around native readiness.

use super::*;

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    #[allow(clippy::too_many_arguments, reason = "exact actor suspension contract")]
    pub(in crate::physical) fn emit_actor_ask(
        &self,
        actor: ActorId,
        message: u32,
        policy: hew_types::actor_delivery::SendPolicy,
        deadline_ns: Option<i64>,
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
        let operation =
            self.emit_actor_call_start(actor, message, policy, deadline_ns, sealed, args)?;
        let ArgumentTransfer::Borrow(target) = args[0] else {
            return Err(CodegenError::FailClosed(
                "ask must borrow its target".into(),
            ));
        };
        // A deadline is an independent progress path, so this call alone
        // cannot prove a closed actor dependency cycle.
        let wait_edge = if deadline_ns.is_some() {
            self.ctx.ptr_type(AddressSpace::default()).const_null()
        } else {
            self.new_actor_wait_edge(self.load_actor_target(target, "ask.wait.target")?.into(), 0)?
        };
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
        frame.suspend(self.ctx, self.llvm, &self.builder, poll, destroyed, false)?;
        self.builder.position_at_end(destroyed);
        self.builder
            .build_store(frame.destroying, self.ctx.bool_type().const_int(1, false))
            .llvm_ctx("mark destroyed completion frame")?;
        self.builder
            .build_unconditional_branch(cancelled)
            .llvm_ctx("abandon destroyed completion")?;
        self.builder.position_at_end(completed);
        self.free_handle("hew_actor_wait_edge_free", wait_edge)?;
        self.emit_actor_call_take(operation, actor, message, policy, target, result)?;
        self.emit_result_edge(Some(result), normal)?;
        self.builder.position_at_end(cancelled);
        self.free_handle("hew_actor_wait_edge_free", wait_edge)?;
        self.free_handle("hew_actor_call_free", operation)?;
        self.initialize_cancellation_fault()?;
        self.emit_edge(cancel)?;
        self.builder.position_at_end(cycle);
        self.initialize_actor_cycle_fault(wait_edge)?;
        self.free_handle("hew_actor_wait_edge_free", wait_edge)?;
        self.free_handle("hew_actor_call_free", operation)?;
        self.emit_edge(unwind)?;
        self.builder.position_at_end(failed);
        self.free_handle("hew_actor_wait_edge_free", wait_edge)?;
        self.free_handle("hew_actor_call_free", operation)?;
        self.initialize_active_fault(HEW_TRAP_USER_PANIC)?;
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
        deadline_ns: Option<i64>,
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
                    self.destroy_value(*source, action)?;
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
            self.ctx
                .i64_type()
                .const_int(deadline_ns.unwrap_or(0) as u64, true)
                .into(),
            self.ctx
                .i32_type()
                .const_int(u64::from(deadline_ns.is_some()), false)
                .into(),
            self.ctx
                .i32_type()
                .const_int(
                    u64::from(policy == hew_types::actor_delivery::SendPolicy::Reject),
                    false,
                )
                .into(),
        ]);
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
    /// as an ordinary call. The operation releases only what was not taken.
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
        self.free_handle("hew_actor_call_free", operation)?;
        let done = self.ctx.append_basic_block(self.value, "ask.taken");
        if policy == hew_types::actor_delivery::SendPolicy::Reject {
            let rejected = self.ctx.append_basic_block(self.value, "ask.rejected");
            let replied = self.ctx.append_basic_block(self.value, "ask.replied");
            let refused = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    status,
                    self.ctx.i32_type().const_int(
                        hew_runtime::internal::types::AskError::MailboxFull as u64,
                        false,
                    ),
                    "ask.refused",
                )
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
            self.emit_ask_refused(result, target, message, request)?;
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

    /// A `policy(target, on_full: .Reject)` call whose destination mailbox is
    /// full: nothing was accepted, so `Rejected` returns the refusal reason
    /// and the original owned request. This is the only refusal a completion
    /// call reports; every
    /// other outcome means the request was accepted or its fate is unknown.
    fn emit_ask_refused(
        &self,
        result: StorageId,
        target: StorageId,
        message: u32,
        request: PointerValue<'ctx>,
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
        let reason = self.actor_unit_variant(&reason_ty, self.ctx.i32_type().const_zero())?;
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

    fn ask_record(
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

    fn emit_ask_result(
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
        let error_ty = &glue.variants[1].fields[0].ty;
        let error = self.ctx.append_basic_block(self.value, "ask.error");
        let done = self.ctx.append_basic_block(self.value, "ask.result");
        // A completion call on a void handler carries no reply payload: the
        // handler's return is the unit reply, so success writes `Ok(())`.
        let completion = reply.is_none() && handler.return_ty == ResolvedTy::Unit;
        if let Some(reply) = reply {
            let success = self.ctx.append_basic_block(self.value, "ask.success");
            let ok = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    status,
                    self.ctx.i32_type().const_zero(),
                    "ask.ok",
                )
                .llvm_ctx("classify typed reply")?;
            self.builder
                .build_conditional_branch(ok, success, error)
                .llvm_ctx("materialize fallible reply")?;
            self.builder.position_at_end(success);
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
                self.write_variant_value(self.slots[result.0 as usize], 0, &[value], glue.id)?;
                self.builder
                    .build_unconditional_branch(done)
                    .llvm_ctx("finish successful reply")?;
            } else {
                self.emit_declared_failure_reply(result, reply, handler, error_ty, glue.id, done)?;
            }
        } else if completion {
            let success = self.ctx.append_basic_block(self.value, "ask.success");
            let ok = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    status,
                    self.ctx.i32_type().const_zero(),
                    "ask.ok",
                )
                .llvm_ctx("classify completion")?;
            self.builder
                .build_conditional_branch(ok, success, error)
                .llvm_ctx("materialize completion outcome")?;
            self.builder.position_at_end(success);
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
            self.write_variant_value(self.slots[result.0 as usize], 0, unit.as_slice(), glue.id)?;
            self.builder
                .build_unconditional_branch(done)
                .llvm_ctx("finish completed call")?;
        } else {
            self.builder
                .build_unconditional_branch(error)
                .llvm_ctx("materialize admission error")?;
        }
        self.builder.position_at_end(error);
        let translate = coro::external(
            self.llvm,
            "hew_ask_error_translate_for_public_result",
            self.ctx
                .i32_type()
                .fn_type(&[self.ctx.i32_type().into()], false),
        )?;
        let tag = call_value(&self.builder, translate, &[status.into()], "ask.error.tag")?
            .into_int_value();
        let error_value = self.actor_unit_variant(error_ty, tag)?;
        self.write_variant_value(self.slots[result.0 as usize], 1, &[error_value], glue.id)?;
        self.builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish ask error")?;
        self.builder.position_at_end(done);
        Ok(())
    }

    /// Unwrap a `fails` handler's `Result<R, E>` reply into the call envelope:
    /// `Ok(r)` is the call's own `Ok`, and `Err(e)` is `ActorError.Failed(e)`.
    fn emit_declared_failure_reply(
        &self,
        result: StorageId,
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
        let wire_layout = self.value_emitter().variant_layout(&handler.return_ty)?;
        let object = self
            .value_emitter()
            .variant_object_ptr(reply, wire_layout)?;
        let tag = self.value_emitter().load_variant_tag(object, wire_layout)?;
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
        let payload_ptr = |emitter: &Self| -> CodegenResult<PointerValue<'ctx>> {
            emitter
                .value_emitter()
                .variant_payload_ptr(object, wire_layout)
        };
        for (block, wire_variant, call_variant) in [(replied, 0_usize, 0_u32), (failed, 1, 1)] {
            self.builder.position_at_end(block);
            let payload_ty =
                llvm_type(self.ctx, &wire_layout.variants[wire_variant].repr)?.into_struct_type();
            let payload = self
                .builder
                .build_load(payload_ty, payload_ptr(self)?, "ask.reply.payload")
                .llvm_ctx("read the declared reply payload")?
                .into_struct_value();
            let field = self
                .builder
                .build_extract_value(payload, 0, "ask.reply.field")
                .llvm_ctx("take the declared reply field")?;
            let value = if call_variant == 0 {
                field
            } else {
                let object_ty = llvm_type(
                    self.ctx,
                    &self.value_emitter().variant_layout(error_ty)?.object.repr,
                )?
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
            self.write_variant_value(
                self.slots[result.0 as usize],
                call_variant,
                &[value],
                glue_id,
            )?;
            self.builder
                .build_unconditional_branch(done)
                .llvm_ctx("finish the declared reply arm")?;
        }
        Ok(())
    }
}
