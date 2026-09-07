//! Exact request transfer and typed reply materialization around native readiness.

use super::*;

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    #[allow(
        clippy::too_many_arguments,
        clippy::too_many_lines,
        reason = "one physical suspension owns request transfer, readiness and all cleanup edges"
    )]
    pub(in crate::physical) fn emit_actor_ask(
        &self,
        actor: ActorId,
        message: u32,
        deadline_ns: Option<i64>,
        args: &[ArgumentTransfer],
        result: StorageId,
        normal: &PhysicalEdge,
        cancel: &PhysicalEdge,
        unwind: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("ask requires a resumable invocation".into())
        })?;
        let actor = self
            .module
            .actors
            .get(actor.0 as usize)
            .ok_or_else(|| CodegenError::FailClosed("ask lacks its actor descriptor".into()))?;
        let handler = actor
            .handlers
            .iter()
            .find(|handler| handler.message_id == message)
            .ok_or_else(|| CodegenError::FailClosed("ask lacks its exact receive member".into()))?;
        let sources = args
            .iter()
            .map(|argument| match argument {
                ArgumentTransfer::Move(source) => Ok(*source),
                _ => Err(CodegenError::FailClosed(
                    "ask lacks an owning request transfer".into(),
                )),
            })
            .collect::<CodegenResult<Vec<_>>>()?;
        let target = TargetData::create(&self.module.target.data_layout);
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let wrapper_ty = message_type(self.module, self.ctx, handler)?;
        let size = target.get_abi_size(&wrapper_ty);
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
        let allocation_failed = self
            .ctx
            .append_basic_block(self.value, "ask.allocation.failed");
        let submit = self.ctx.append_basic_block(self.value, "ask.submit");
        let missing = self
            .builder
            .build_is_null(wrapper, "ask.missing.request")
            .llvm_ctx("check request allocation")?;
        self.builder
            .build_conditional_branch(missing, allocation_failed, submit)
            .llvm_ctx("retain request fields until allocation")?;
        self.builder.position_at_end(allocation_failed);
        for source in &sources {
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
        self.emit_ask_result(
            result,
            self.ctx.i32_type().const_int(
                hew_runtime::internal::types::AskError::SendFailed as u64,
                false,
            ),
            None,
            handler,
        )?;
        self.emit_result_edge(Some(result), normal)?;
        self.builder.position_at_end(submit);
        self.builder
            .build_store(wrapper, self.ctx.i8_type().const_int(1, false))
            .llvm_ctx("initialize request ownership")?;
        for (index, source) in sources.iter().skip(1).enumerate() {
            let field = self
                .builder
                .build_struct_gep(wrapper_ty, wrapper, (index + 1) as u32, "ask.request.field")
                .llvm_ctx("address typed request field")?;
            self.builder
                .build_store(field, self.load(*source, "ask.argument")?)
                .llvm_ctx("transfer request field")?;
        }
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
        let new = coro::external(
            self.llvm,
            "hew_reply_channel_new_native",
            ptr.fn_type(&[ptr.into(); 2], false),
        )?;
        let channel = call_value(
            &self.builder,
            new,
            &[waker.into(), drop_reply.into()],
            "ask.channel",
        )?
        .into_pointer_value();
        let timer = deadline_ns
            .map(|duration| {
                let start = coro::external(
                    self.llvm,
                    "hew_coro_sleep_new",
                    ptr.fn_type(&[self.ctx.i64_type().into(), ptr.into()], false),
                )?;
                Ok::<_, CodegenError>(
                    call_value(
                        &self.builder,
                        start,
                        &[
                            self.ctx.i64_type().const_int(duration as u64, true).into(),
                            waker.into(),
                        ],
                        "ask.deadline",
                    )?
                    .into_pointer_value(),
                )
            })
            .transpose()?;
        let drop_request = self
            .llvm
            .get_function(&message_symbol(actor.id, message))
            .ok_or_else(|| CodegenError::FailClosed("ask request lacks its destructor".into()))?;
        let wait_edge = self.new_actor_wait_edge(
            self.load_actor_target(sources[0], "ask.wait.target")?
                .into(),
            0,
        )?;
        let cycle = self.ctx.append_basic_block(self.value, "ask.cycle.fault");
        let submit_fn = coro::external(
            self.llvm,
            "hew_actor_ask_submit_native",
            self.ctx.i32_type().fn_type(
                &[
                    size_ty.into(),
                    self.ctx.i32_type().into(),
                    ptr.into(),
                    size_ty.into(),
                    ptr.into(),
                    ptr.into(),
                ],
                false,
            ),
        )?;
        let submitted = call_value(
            &self.builder,
            submit_fn,
            &[
                self.load_actor_target(sources[0], "ask.target")?.into(),
                self.ctx
                    .i32_type()
                    .const_int(u64::from(message), false)
                    .into(),
                wrapper.into(),
                size_ty.const_int(size, false).into(),
                drop_request.as_global_value().as_pointer_value().into(),
                channel.into(),
            ],
            "ask.admission",
        )?
        .into_int_value();
        for source in &sources {
            self.clear_owned(*source)?;
        }
        let reply_layout = callable(self.module, handler.callable)?
            .return_layout
            .as_ref();
        // The reply slot is hoisted out of loops, then promoted into the coroutine
        // frame if a request parks. No operation retains its address after take.
        let reply = reply_layout
            .map(|layout| {
                let entry = self.ctx.create_builder();
                let block = self.value.get_first_basic_block().unwrap();
                if let Some(first) = block.get_first_instruction() {
                    entry.position_before(&first);
                } else {
                    entry.position_at_end(block);
                }
                entry
                    .build_alloca(llvm_type(self.ctx, &layout.repr)?, "ask.reply")
                    .llvm_ctx("allocate exact reply slot")
            })
            .transpose()?;
        let poll = self.ctx.append_basic_block(self.value, "ask.poll");
        let inspect = self.ctx.append_basic_block(self.value, "ask.inspect");
        let pending = self.ctx.append_basic_block(self.value, "ask.pending");
        let parked = self.ctx.append_basic_block(self.value, "ask.parked");
        let destroyed = self
            .ctx
            .append_basic_block(self.value, "ask.invalid.destroy");
        let completed = self.ctx.append_basic_block(self.value, "ask.completed");
        let rejected = self.ctx.append_basic_block(self.value, "ask.rejected");
        let cancelled = self.ctx.append_basic_block(self.value, "ask.cancelled");
        let failed = self.ctx.append_basic_block(self.value, "ask.failed");
        let admitted = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                submitted,
                self.ctx.i32_type().const_zero(),
                "ask.admitted",
            )
            .llvm_ctx("check admission")?;
        self.builder
            .build_conditional_branch(admitted, poll, rejected)
            .llvm_ctx("select admitted request")?;
        self.builder.position_at_end(rejected);
        self.close_ask(channel, timer, wait_edge)?;
        self.emit_ask_result(result, submitted, None, handler)?;
        self.emit_result_edge(Some(result), normal)?;
        self.builder.position_at_end(poll);
        self.free_handle("hew_actor_wait_edge_prepare", wait_edge)?;
        let cancellation = self.state_value("hew_coro_state_is_cancelled", frame.state)?;
        let cancellation = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                cancellation,
                self.ctx.i32_type().const_zero(),
                "ask.cancel.requested",
            )
            .llvm_ctx("inspect caller cancellation")?;
        self.builder
            .build_conditional_branch(cancellation, cancelled, inspect)
            .llvm_ctx("select caller cancellation")?;
        self.builder.position_at_end(inspect);
        let poll_fn = coro::external(
            self.llvm,
            "hew_reply_channel_poll_native",
            self.ctx
                .i32_type()
                .fn_type(&[ptr.into(), size_ty.into(), ptr.into()], false),
        )?;
        let status = call_value(
            &self.builder,
            poll_fn,
            &[
                channel.into(),
                size_ty
                    .const_int(reply_layout.map_or(0, |layout| layout.size), false)
                    .into(),
                reply.unwrap_or(ptr.const_null()).into(),
            ],
            "ask.outcome",
        )?
        .into_int_value();
        let waiting = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_all_ones(),
                "ask.waiting",
            )
            .llvm_ctx("inspect reply readiness")?;
        self.builder
            .build_conditional_branch(waiting, pending, completed)
            .llvm_ctx("select reply readiness")?;
        self.builder.position_at_end(pending);
        if let Some(timer) = timer {
            let timed_out = self.ctx.append_basic_block(self.value, "ask.timed.out");
            let timer_status = self.state_value("hew_coro_sleep_status", timer)?;
            self.builder
                .build_switch(
                    timer_status,
                    failed,
                    &[
                        (self.ctx.i32_type().const_zero(), parked),
                        (self.ctx.i32_type().const_int(1, false), timed_out),
                    ],
                )
                .llvm_ctx("inspect ask deadline")?;
            self.builder.position_at_end(timed_out);
            self.close_ask(channel, Some(timer), wait_edge)?;
            self.emit_ask_result(
                result,
                self.ctx.i32_type().const_int(
                    hew_runtime::internal::types::AskError::Timeout as u64,
                    false,
                ),
                None,
                handler,
            )?;
            self.emit_result_edge(Some(result), normal)?;
        } else {
            self.builder
                .build_unconditional_branch(parked)
                .llvm_ctx("park pending ask")?;
        }
        self.builder.position_at_end(parked);
        self.check_actor_wait_cycle(wait_edge, cycle)?;
        frame.suspend(self.ctx, self.llvm, &self.builder, poll, destroyed, false)?;
        self.builder.position_at_end(destroyed);
        self.reject_invalid_task_state()?;
        self.builder.position_at_end(completed);
        self.close_ask(channel, timer, wait_edge)?;
        self.emit_ask_result(result, status, reply, handler)?;
        self.emit_result_edge(Some(result), normal)?;
        self.builder.position_at_end(cancelled);
        self.close_ask(channel, timer, wait_edge)?;
        self.initialize_cancellation_fault()?;
        self.emit_edge(cancel)?;
        self.builder.position_at_end(cycle);
        self.initialize_actor_cycle_fault(wait_edge)?;
        self.close_ask(channel, timer, wait_edge)?;
        self.emit_edge(unwind)?;
        self.builder.position_at_end(failed);
        self.close_ask(channel, timer, wait_edge)?;
        self.initialize_active_fault(HEW_TRAP_USER_PANIC)?;
        self.emit_edge(unwind)
    }

    fn close_ask(
        &self,
        channel: PointerValue<'ctx>,
        timer: Option<PointerValue<'ctx>>,
        wait_edge: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        self.free_handle("hew_actor_wait_edge_free", wait_edge)?;
        if let Some(timer) = timer {
            self.free_handle("hew_coro_sleep_free", timer)?;
        }
        self.free_handle("hew_reply_channel_cancel", channel)?;
        self.free_handle("hew_reply_channel_free", channel)
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
            let layout = self
                .module
                .target
                .layout(&handler.return_ty)
                .ok_or_else(|| CodegenError::FailClosed("reply lacks its target layout".into()))?;
            let value = self
                .builder
                .build_load(llvm_type(self.ctx, &layout.repr)?, reply, "ask.reply.value")
                .llvm_ctx("take typed reply")?;
            self.write_variant_value(self.slots[result.0 as usize], 0, &[value], glue.id)?;
            self.builder
                .build_unconditional_branch(done)
                .llvm_ctx("finish successful reply")?;
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
}
