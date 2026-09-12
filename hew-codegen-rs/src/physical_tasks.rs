//! Native structured tasks consume the physical scope and transfer contracts.

use super::*;
use hew_mir::physical::TaskScopeId;

fn result_descriptor(callable: CallableId, storage: StorageId) -> String {
    format!("__hew_task_result_{}_{}", callable.0, storage.0)
}

impl ModuleEmitter<'_, '_> {
    pub(super) fn emit_task_descriptors(&self) -> CodegenResult<()> {
        for function in &self.module.functions {
            for op in function.blocks.iter().flat_map(|block| &block.ops) {
                if let PhysicalOp::TaskSpawn {
                    dest,
                    output: Some(output),
                    ..
                } = op
                {
                    self.emit_value_descriptor(
                        &result_descriptor(function.callable, *dest),
                        output,
                    )?;
                }
            }
        }
        Ok(())
    }
}

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    pub(super) fn task_pointer_call(
        &self,
        name: &str,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) -> CodegenResult<PointerValue<'ctx>> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let function = coro::external(
            self.llvm,
            name,
            pointer.fn_type(&vec![pointer.into(); args.len()], false),
        )?;
        Ok(suspend::call_value(&self.builder, function, args, name)?.into_pointer_value())
    }

    fn task_frame(&self) -> CodegenResult<&coro::Frame<'ctx>> {
        self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("task operation requires its resumable invocation".into())
        })
    }

    fn task_scope_handle(&self, scope: TaskScopeId) -> CodegenResult<PointerValue<'ctx>> {
        let slot = self
            .task_scopes
            .get(&scope)
            .ok_or_else(|| CodegenError::FailClosed("task scope has no declared storage".into()))?;
        Ok(self
            .builder
            .build_load(
                self.ctx.ptr_type(AddressSpace::default()),
                *slot,
                "task.scope",
            )
            .llvm_ctx("load task scope")?
            .into_pointer_value())
    }

    pub(super) fn emit_task_scope_enter(
        &self,
        scope: TaskScopeId,
        parent: Option<TaskScopeId>,
        duration: Option<StorageId>,
    ) -> CodegenResult<()> {
        let token = if let Some(parent) = parent {
            self.task_pointer_call(
                "hew_task_scope_cancel_token",
                &[self.task_scope_handle(parent)?.into()],
            )?
        } else {
            self.task_pointer_call("hew_coro_state_token", &[self.task_frame()?.state.into()])?
        };
        let handle = self.task_pointer_call("hew_checked_scope_new", &[token.into()])?;
        self.builder
            .build_store(self.task_scopes[&scope], handle)
            .llvm_ctx("retain lexical task scope")?;
        let token = self.task_pointer_call("hew_task_scope_cancel_token", &[handle.into()])?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let enter = coro::external(
            self.llvm,
            "hew_coro_state_enter_token",
            self.ctx.void_type().fn_type(&[pointer.into(); 2], false),
        )?;
        self.builder
            .build_call(enter, &[self.task_frame()?.state.into(), token.into()], "")
            .llvm_ctx("enter lexical cancellation token")?;
        if let Some(duration) = duration {
            let arm = coro::external(
                self.llvm,
                "hew_checked_scope_deadline",
                self.ctx
                    .void_type()
                    .fn_type(&[pointer.into(), self.ctx.i64_type().into()], false),
            )?;
            self.builder
                .build_call(
                    arm,
                    &[handle.into(), self.load(duration, "scope.duration")?.into()],
                    "",
                )
                .llvm_ctx("arm lexical deadline")?;
        }
        Ok(())
    }

    pub(super) fn emit_task_scope_close(&self, scope: TaskScopeId) -> CodegenResult<()> {
        self.free_handle("hew_coro_state_leave_token", self.task_frame()?.state)?;
        self.free_handle("hew_checked_scope_close", self.task_scope_handle(scope)?)
    }

    pub(super) fn emit_task_spawn(
        &self,
        scope: TaskScopeId,
        callable: StorageId,
        dest: StorageId,
    ) -> CodegenResult<()> {
        let descriptor = self
            .llvm
            .get_global(&result_descriptor(self.function.callable, dest))
            .map(|global| global.as_pointer_value())
            .or_else(|| {
                matches!(&self.function.storage[dest.0 as usize].ty,
                ResolvedTy::Task(output) if **output == ResolvedTy::Never)
                .then(|| self.ctx.ptr_type(AddressSpace::default()).const_null())
            })
            .ok_or_else(|| {
                CodegenError::FailClosed("task result lacks its exact value descriptor".into())
            })?;
        let task = self.task_pointer_call(
            "hew_checked_task_spawn",
            &[
                self.task_scope_handle(scope)?.into(),
                self.slots[callable.0 as usize].into(),
                descriptor.into(),
            ],
        )?;
        self.clear_owned(callable)?;
        self.store(dest, task.into())
    }

    /// Premature frame destruction and a successful uninhabited result both
    /// violate the checked task ABI.
    pub(super) fn reject_invalid_task_state(&self) -> CodegenResult<()> {
        let abort = coro::external(self.llvm, "abort", self.ctx.void_type().fn_type(&[], false))?;
        self.builder
            .build_call(abort, &[], "")
            .llvm_ctx("reject an invalid task state")?;
        self.builder
            .build_unreachable()
            .llvm_ctx("terminate invalid frame owner")?;
        Ok(())
    }

    #[allow(
        clippy::too_many_lines,
        reason = "one task wait owns registration, cancellation, drain and value transfer"
    )]
    pub(super) fn emit_task_await(
        &self,
        task: &ArgumentTransfer,
        result: Option<StorageId>,
        normal: &Option<PhysicalEdge>,
        cancel: &PhysicalEdge,
        unwind: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let ArgumentTransfer::Move(task) = task else {
            return Err(CodegenError::FailClosed(
                "await must consume its task handle".into(),
            ));
        };
        let frame = self.task_frame()?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let waker = self.task_pointer_call("hew_coro_state_waker", &[frame.state.into()])?;
        let task_value = self.load(*task, "await.task")?;
        let wait = self.task_pointer_call(
            "hew_checked_task_wait_new",
            &[task_value.into(), waker.into()],
        )?;
        self.clear_owned(*task)?;
        let poll = self.ctx.append_basic_block(self.value, "await.poll");
        let request_cancel = self
            .ctx
            .append_basic_block(self.value, "await.request.cancel");
        let inspect = self.ctx.append_basic_block(self.value, "await.inspect");
        let pending = self.ctx.append_basic_block(self.value, "await.pending");
        let destroyed = self
            .ctx
            .append_basic_block(self.value, "await.invalid.destroy");
        let complete = self.ctx.append_basic_block(self.value, "await.complete");
        let take = self.ctx.append_basic_block(self.value, "await.take");
        let abandoned = self.ctx.append_basic_block(self.value, "await.abandoned");
        let value = self.ctx.append_basic_block(self.value, "await.value");
        let failure = self.ctx.append_basic_block(self.value, "await.fault");
        let cancelled = self.ctx.append_basic_block(self.value, "await.cancelled");
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("poll task")?;
        self.builder.position_at_end(poll);
        let cancellation = self.state_value("hew_coro_state_is_cancelled", frame.state)?;
        let cancellation = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                cancellation,
                self.ctx.i32_type().const_zero(),
                "await.cancel.requested",
            )
            .llvm_ctx("check parent cancellation")?;
        self.builder
            .build_conditional_branch(cancellation, request_cancel, inspect)
            .llvm_ctx("request task cancellation")?;
        self.builder.position_at_end(request_cancel);
        self.free_handle("hew_checked_task_wait_cancel", wait)?;
        self.builder
            .build_unconditional_branch(inspect)
            .llvm_ctx("drain cancelled task")?;
        self.builder.position_at_end(inspect);
        let status = self.state_value("hew_checked_task_wait_status", wait)?;
        let ready = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                status,
                self.ctx.i32_type().const_zero(),
                "await.ready",
            )
            .llvm_ctx("check task completion")?;
        self.builder
            .build_conditional_branch(ready, complete, pending)
            .llvm_ctx("wait for task cleanup")?;
        self.builder.position_at_end(pending);
        frame.suspend(self.ctx, self.llvm, &self.builder, poll, destroyed, false)?;
        self.builder.position_at_end(destroyed);
        self.reject_invalid_task_state()?;
        self.builder.position_at_end(complete);
        self.builder
            .build_conditional_branch(cancellation, abandoned, take)
            .llvm_ctx("choose task result transfer")?;
        self.builder.position_at_end(abandoned);
        self.free_handle("hew_checked_task_wait_free", wait)?;
        self.initialize_cancellation_fault()?;
        self.emit_edge(cancel)?;
        self.builder.position_at_end(take);
        let private_status = self.state_value("hew_checked_task_wait_private_status", wait)?;
        let output = result.map_or(pointer.const_null(), |result| self.slots[result.0 as usize]);
        let take_fn = coro::external(
            self.llvm,
            "hew_checked_task_wait_take",
            self.ctx.i32_type().fn_type(&[pointer.into(); 3], false),
        )?;
        let taken = suspend::call_value(
            &self.builder,
            take_fn,
            &[wait.into(), output.into(), self.active_fault.into()],
            "await.outcome",
        )?
        .into_int_value();
        self.free_handle("hew_checked_task_wait_free", wait)?;
        self.builder
            .build_store(self.active_status, private_status)
            .llvm_ctx("retain child fault status")?;
        self.builder
            .build_switch(
                taken,
                failure,
                &[
                    (self.ctx.i32_type().const_int(1, false), value),
                    (self.ctx.i32_type().const_int(3, false), cancelled),
                ],
            )
            .llvm_ctx("dispatch task outcome")?;
        self.builder.position_at_end(value);
        if let Some(normal) = normal {
            self.emit_result_edge(result, normal)?;
        } else {
            self.reject_invalid_task_state()?;
        }
        self.builder.position_at_end(cancelled);
        self.emit_edge(cancel)?;
        self.builder.position_at_end(failure);
        self.emit_edge(unwind)
    }

    #[allow(
        clippy::too_many_lines,
        reason = "scope drain preserves its primary fault across every child completion"
    )]
    pub(super) fn emit_task_scope_join(
        &self,
        scope: TaskScopeId,
        mode: hew_mir::physical::TaskScopeJoinMode,
        normal: &PhysicalEdge,
        unwind: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let frame = self.task_frame()?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let handle = self.task_scope_handle(scope)?;
        let waker = self.task_pointer_call("hew_coro_state_waker", &[frame.state.into()])?;
        let wait =
            self.task_pointer_call("hew_checked_scope_wait_new", &[handle.into(), waker.into()])?;
        if mode.cancels_losers() {
            self.free_handle("hew_checked_scope_wait_cancel_losers", wait)?;
        } else if mode.preserves_fault() {
            self.free_handle("hew_checked_scope_cancel", handle)?;
        }
        let poll = self.ctx.append_basic_block(self.value, "scope.poll");
        let request = self
            .ctx
            .append_basic_block(self.value, "scope.request.cancel");
        let inspect = self.ctx.append_basic_block(self.value, "scope.inspect");
        let pending = self.ctx.append_basic_block(self.value, "scope.pending");
        let destroyed = self
            .ctx
            .append_basic_block(self.value, "scope.invalid.destroy");
        let complete = self.ctx.append_basic_block(self.value, "scope.complete");
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("poll scope drain")?;
        self.builder.position_at_end(poll);
        let cancellation = self.state_value("hew_coro_state_is_cancelled", frame.state)?;
        let cancellation = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                cancellation,
                self.ctx.i32_type().const_zero(),
                "scope.cancel.requested",
            )
            .llvm_ctx("check parent cancellation")?;
        self.builder
            .build_conditional_branch(cancellation, request, inspect)
            .llvm_ctx("select scope cancellation")?;
        self.builder.position_at_end(request);
        self.free_handle("hew_checked_scope_cancel", handle)?;
        self.builder
            .build_unconditional_branch(inspect)
            .llvm_ctx("drain scope cancellation")?;
        self.builder.position_at_end(inspect);
        let ready = self.state_value("hew_checked_scope_wait_status", wait)?;
        let ready = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                ready,
                self.ctx.i32_type().const_zero(),
                "scope.ready",
            )
            .llvm_ctx("check scope completion")?;
        self.builder
            .build_conditional_branch(ready, complete, pending)
            .llvm_ctx("wait for all child cleanup")?;
        self.builder.position_at_end(pending);
        frame.suspend(self.ctx, self.llvm, &self.builder, poll, destroyed, false)?;
        self.builder.position_at_end(destroyed);
        self.reject_invalid_task_state()?;
        self.builder.position_at_end(complete);
        let child_fault_slot = self
            .builder
            .build_alloca(pointer, "scope.child.fault")
            .llvm_ctx("allocate drained fault slot")?;
        self.builder
            .build_store(child_fault_slot, pointer.const_null())
            .llvm_ctx("initialize drained fault slot")?;
        let take = coro::external(
            self.llvm,
            "hew_checked_scope_wait_take_fault",
            self.ctx.i32_type().fn_type(&[pointer.into(); 2], false),
        )?;
        let child_status = suspend::call_value(
            &self.builder,
            take,
            &[wait.into(), child_fault_slot.into()],
            "scope.child.status",
        )?;
        self.free_handle("hew_checked_scope_wait_free", wait)?;
        let primary = self
            .builder
            .build_load(pointer, self.active_fault, "scope.primary")
            .llvm_ctx("load primary fault")?
            .into_pointer_value();
        let child_fault = self
            .builder
            .build_load(pointer, child_fault_slot, "scope.child")
            .llvm_ctx("load child fault")?;
        let primary_status = self
            .builder
            .build_load(
                self.ctx.i32_type(),
                self.active_status,
                "scope.primary.status",
            )
            .llvm_ctx("load primary status")?;
        let present = self
            .builder
            .build_is_not_null(primary, "scope.primary.present")
            .llvm_ctx("test primary fault")?;
        let status = self
            .builder
            .build_select(present, primary_status, child_status, "scope.status")
            .llvm_ctx("preserve first fault status")?;
        let combined =
            self.task_pointer_call("hew_fault_combine", &[primary.into(), child_fault.into()])?;
        self.builder
            .build_store(self.active_fault, combined)
            .llvm_ctx("store combined scope fault")?;
        self.builder
            .build_store(self.active_status, status)
            .llvm_ctx("store combined scope status")?;
        if mode.preserves_fault() {
            return self.emit_edge(normal);
        }
        let check_cancel = self
            .ctx
            .append_basic_block(self.value, "scope.check.cancel");
        let cancelled = self.ctx.append_basic_block(self.value, "scope.cancelled");
        let failure = self.ctx.append_basic_block(self.value, "scope.failed");
        let success = self.ctx.append_basic_block(self.value, "scope.success");
        let failed = self
            .builder
            .build_is_not_null(combined, "scope.failed")
            .llvm_ctx("test drained fault")?;
        self.builder
            .build_conditional_branch(failed, failure, check_cancel)
            .llvm_ctx("dispatch drained fault")?;
        self.builder.position_at_end(check_cancel);
        self.builder
            .build_conditional_branch(cancellation, cancelled, success)
            .llvm_ctx("dispatch parent cancellation")?;
        self.builder.position_at_end(cancelled);
        self.initialize_cancellation_fault()?;
        self.emit_edge(unwind)?;
        self.builder.position_at_end(failure);
        self.emit_edge(unwind)?;
        self.builder.position_at_end(success);
        self.emit_edge(normal)
    }
}
