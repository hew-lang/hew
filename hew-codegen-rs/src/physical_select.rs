//! Resumable readiness observation; selected branches own task result transfer.

use super::*;

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    #[allow(
        clippy::too_many_lines,
        reason = "one selection owns observation registration, polling and every detach edge"
    )]
    pub(super) fn emit_task_select(
        &self,
        tasks: &[ArgumentTransfer],
        timeout: Option<StorageId>,
        result: StorageId,
        normal: &PhysicalEdge,
        cancel: &PhysicalEdge,
        unwind: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("task selection requires a resumable body".into())
        })?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let target = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let count = u32::try_from(tasks.len()).map_err(|_| {
            CodegenError::FailClosed("selection exceeds its task operand capacity".into())
        })?;
        // The runtime copies these handles and retains each observation during
        // construction. Hoist this fixed scratch array so repeated selection
        // in a loop reuses storage; no borrowed array pointer survives new().
        let task_array = if tasks.is_empty() {
            pointer.const_null()
        } else {
            let entry = self.value.get_first_basic_block().ok_or_else(|| {
                CodegenError::FailClosed("selection has no function entry".into())
            })?;
            let scratch = self.ctx.create_builder();
            if let Some(first) = entry.get_first_instruction() {
                scratch.position_before(&first);
            } else {
                scratch.position_at_end(entry);
            }
            let array_ty = pointer.array_type(count);
            let array = scratch
                .build_alloca(array_ty, "select.tasks")
                .llvm_ctx("allocate selection task scratch")?;
            for (index, task) in tasks.iter().enumerate() {
                let ArgumentTransfer::Borrow(task) = task else {
                    return Err(CodegenError::FailClosed(
                        "selection must borrow every task handle".into(),
                    ));
                };
                // SAFETY: the static array has exactly one slot per task.
                let slot = unsafe {
                    self.builder.build_in_bounds_gep(
                        array_ty,
                        array,
                        &[
                            self.ctx.i32_type().const_zero(),
                            self.ctx.i32_type().const_int(index as u64, false),
                        ],
                        "select.task.slot",
                    )
                }
                .llvm_ctx("address selected task")?;
                self.builder
                    .build_store(slot, self.load(*task, "select.task")?)
                    .llvm_ctx("borrow selected task")?;
            }
            array
        };
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
            pointer.fn_type(
                &[
                    pointer.into(),
                    size_ty.into(),
                    self.ctx.i32_type().into(),
                    self.ctx.i64_type().into(),
                    pointer.into(),
                ],
                false,
            ),
        )?;
        let duration = match timeout {
            Some(duration) => self.load(duration, "select.duration")?,
            None => self.ctx.i64_type().const_zero().into(),
        };
        let operation = suspend::call_value(
            &self.builder,
            start,
            &[
                task_array.into(),
                size_ty.const_int(u64::from(count), false).into(),
                self.ctx
                    .i32_type()
                    .const_int(u64::from(timeout.is_some()), false)
                    .into(),
                duration.into(),
                waker.into(),
            ],
            "select.operation",
        )?
        .into_pointer_value();
        let poll = self.ctx.append_basic_block(self.value, "select.poll");
        let inspect = self.ctx.append_basic_block(self.value, "select.inspect");
        let outcome = self.ctx.append_basic_block(self.value, "select.outcome");
        let pending = self.ctx.append_basic_block(self.value, "select.pending");
        let completed = self.ctx.append_basic_block(self.value, "select.completed");
        let cancelled = self.ctx.append_basic_block(self.value, "select.cancelled");
        let destroyed = self.ctx.append_basic_block(self.value, "select.destroyed");
        let failed = self.ctx.append_basic_block(self.value, "select.failed");
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
            "hew_checked_task_select_poll",
            self.ctx.i64_type().fn_type(&[pointer.into()], false),
        )?;
        let index =
            suspend::call_value(&self.builder, poll_fn, &[operation.into()], "select.index")?
                .into_int_value();
        self.builder
            .build_switch(
                index,
                outcome,
                &[(self.ctx.i64_type().const_all_ones(), pending)],
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
                    .const_int(u64::from(count) + u64::from(timeout.is_some()), false),
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
        self.builder.position_at_end(failed);
        self.free_handle("hew_checked_task_select_free", operation)?;
        self.initialize_active_fault(HEW_TRAP_USER_PANIC)?;
        self.emit_edge(unwind)
    }
}
