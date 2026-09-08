//! Resumable readiness observation; selected branches own task result transfer.

use super::*;

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    #[allow(
        clippy::too_many_lines,
        clippy::too_many_arguments,
        reason = "one selection owns observation registration, polling and every detach edge"
    )]
    pub(super) fn emit_task_select(
        &self,
        order: hew_mir::physical::TaskSelectionOrder,
        sources: &[hew_mir::physical::PhysicalSelectSource],
        timeout: Option<StorageId>,
        result: StorageId,
        normal: &PhysicalEdge,
        cancel: &PhysicalEdge,
        unwind: &PhysicalEdge,
    ) -> CodegenResult<()> {
        use hew_mir::physical::PhysicalSelectSource;

        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("task selection requires a resumable body".into())
        })?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let count = u64::try_from(sources.len()).map_err(|_| {
            CodegenError::FailClosed("selection exceeds its source operand capacity".into())
        })?;
        let waker_fn = coro::external(
            self.llvm,
            "hew_coro_state_waker",
            pointer.fn_type(&[pointer.into()], false),
        )?;
        let waker = suspend::call_value(
            &self.builder,
            waker_fn,
            &[frame.state.into()],
            "select.waker",
        )?;
        let start = coro::external(
            self.llvm,
            "hew_checked_task_select_new",
            pointer.fn_type(&[pointer.into()], false),
        )?;
        let operation =
            suspend::call_value(&self.builder, start, &[waker.into()], "select.operation")?
                .into_pointer_value();
        let context = coro::external(
            self.llvm,
            "hew_checked_task_select_set_context",
            self.ctx.void_type().fn_type(&[pointer.into(); 2], false),
        )?;
        self.builder
            .build_call(context, &[operation.into(), frame.state.into()], "")
            .llvm_ctx("bind select to its actor turn")?;
        // Registration follows arm order, so a poll result is the arm's index.
        for source in sources {
            let ArgumentTransfer::Borrow(handle) = source.transfer() else {
                return Err(CodegenError::FailClosed(
                    "selection must borrow every source handle".into(),
                ));
            };
            let symbol = match source {
                PhysicalSelectSource::Task(_) => "hew_checked_task_select_add_task",
                PhysicalSelectSource::ChannelRecv(_) => "hew_checked_task_select_add_channel",
                PhysicalSelectSource::ActorCall(_) => "hew_checked_task_select_add_actor",
            };
            let add = coro::external(
                self.llvm,
                symbol,
                self.ctx
                    .void_type()
                    .fn_type(&[pointer.into(), pointer.into()], false),
            )?;
            let handle = self.load(handle, "select.source")?;
            self.builder
                .build_call(add, &[operation.into(), handle.into()], "select.register")
                .llvm_ctx("register a selection source")?;
        }
        // The timer starts only after every observation is registered.
        if let Some(duration) = timeout {
            let arm = coro::external(
                self.llvm,
                "hew_checked_task_select_arm_timer",
                self.ctx
                    .void_type()
                    .fn_type(&[pointer.into(), self.ctx.i64_type().into()], false),
            )?;
            let duration = self.load(duration, "select.duration")?;
            self.builder
                .build_call(
                    arm,
                    &[operation.into(), duration.into()],
                    "select.arm.timer",
                )
                .llvm_ctx("arm the selection timer")?;
        }
        let poll = self.ctx.append_basic_block(self.value, "select.poll");
        let inspect = self.ctx.append_basic_block(self.value, "select.inspect");
        let outcome = self.ctx.append_basic_block(self.value, "select.outcome");
        let pending = self.ctx.append_basic_block(self.value, "select.pending");
        let completed = self.ctx.append_basic_block(self.value, "select.completed");
        let cancelled = self.ctx.append_basic_block(self.value, "select.cancelled");
        let destroyed = self.ctx.append_basic_block(self.value, "select.destroyed");
        let failed = self.ctx.append_basic_block(self.value, "select.failed");
        let cycle = self.ctx.append_basic_block(self.value, "select.cycle");
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("poll selection")?;
        self.builder.position_at_end(poll);
        let cancellation = self.state_value("hew_coro_state_is_cancelled", frame.state)?;
        let cancellation = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                cancellation,
                self.ctx.i32_type().const_zero(),
                "select.cancel.requested",
            )
            .llvm_ctx("test selection cancellation")?;
        self.builder
            .build_conditional_branch(cancellation, cancelled, inspect)
            .llvm_ctx("select cancellation cleanup")?;
        self.builder.position_at_end(inspect);
        let poll_fn = coro::external(
            self.llvm,
            match order {
                hew_mir::physical::TaskSelectionOrder::Source => "hew_checked_task_select_poll",
                hew_mir::physical::TaskSelectionOrder::Completion => {
                    "hew_checked_task_select_poll_first"
                }
            },
            self.ctx.i64_type().fn_type(&[pointer.into()], false),
        )?;
        let index =
            suspend::call_value(&self.builder, poll_fn, &[operation.into()], "select.index")?
                .into_int_value();
        self.builder
            .build_switch(
                index,
                outcome,
                &[
                    (self.ctx.i64_type().const_all_ones(), pending),
                    (self.ctx.i64_type().const_int((-3_i64) as u64, true), cycle),
                ],
            )
            .llvm_ctx("park pending selection")?;
        self.builder.position_at_end(outcome);
        let in_range = self
            .builder
            .build_int_compare(
                IntPredicate::ULT,
                index,
                self.ctx
                    .i64_type()
                    .const_int(count + u64::from(timeout.is_some()), false),
                "select.valid.index",
            )
            .llvm_ctx("validate selected source index")?;
        self.builder
            .build_conditional_branch(in_range, completed, failed)
            .llvm_ctx("select readiness outcome")?;
        self.builder.position_at_end(pending);
        frame.suspend(self.ctx, self.llvm, &self.builder, poll, destroyed, false)?;
        self.builder.position_at_end(destroyed);
        self.builder
            .build_store(frame.destroying, self.ctx.bool_type().const_int(1, false))
            .llvm_ctx("mark destroyed selection frame")?;
        self.builder
            .build_unconditional_branch(cancelled)
            .llvm_ctx("run selection cancellation cleanup")?;
        self.builder.position_at_end(completed);
        self.free_handle("hew_checked_task_select_free", operation)?;
        self.store(result, index.into())?;
        self.emit_edge(normal)?;
        self.builder.position_at_end(cancelled);
        self.free_handle("hew_checked_task_select_free", operation)?;
        self.initialize_cancellation_fault()?;
        self.emit_edge(cancel)?;
        self.builder.position_at_end(cycle);
        let fault = coro::external(
            self.llvm,
            "hew_checked_task_select_fault",
            pointer.fn_type(&[pointer.into()], false),
        )?;
        let fault = suspend::call_value(
            &self.builder,
            fault,
            &[operation.into()],
            "select.cycle.fault",
        )?;
        self.store_active_fault(fault, HEW_TRAP_USER_PANIC)?;
        self.free_handle("hew_checked_task_select_free", operation)?;
        self.emit_edge(unwind)?;
        self.builder.position_at_end(failed);
        self.free_handle("hew_checked_task_select_free", operation)?;
        self.initialize_active_fault(HEW_TRAP_USER_PANIC)?;
        self.emit_edge(unwind)
    }
}
