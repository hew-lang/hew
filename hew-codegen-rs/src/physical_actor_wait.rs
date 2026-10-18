//! Checked mailbox capacity waits retain the source message until admission.

use super::*;
use hew_runtime::actor_native::HewSubmitStatus;

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    pub(super) fn emit_actor_await_restarted(
        &self,
        source: StorageId,
        unwind: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("restart wait requires a resumable invocation".into())
        })?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let wake = get_or_declare_external(
            self.llvm,
            "hew_coro_state_waker",
            ptr.fn_type(&[ptr.into()], false),
        )?;
        let waker = call_value(
            &self.builder,
            wake,
            &[frame.state.into()],
            "restart.wait.waker",
        )?;
        let token = self.load(source, "restart.role")?;
        let new = get_or_declare_external(
            self.llvm,
            "hew_actor_restart_wait_new",
            ptr.fn_type(&[token.get_type().into(), ptr.into()], false),
        )?;
        let wait = call_value(
            &self.builder,
            new,
            &[token.into(), waker.into()],
            "restart.wait",
        )?
        .into_pointer_value();
        let poll = self.ctx.append_basic_block(self.value, "restart.wait.poll");
        let pending = self
            .ctx
            .append_basic_block(self.value, "restart.wait.pending");
        let complete = self
            .ctx
            .append_basic_block(self.value, "restart.wait.complete");
        let cancelled = self
            .ctx
            .append_basic_block(self.value, "restart.wait.cancelled");
        let destroyed = self
            .ctx
            .append_basic_block(self.value, "restart.wait.invalid.destroy");
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("poll role restart")?;
        self.builder.position_at_end(poll);
        let cancellation = self.state_value("hew_coro_state_is_cancelled", frame.state)?;
        let cancelled_now = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                cancellation,
                self.ctx.i32_type().const_zero(),
                "restart.wait.cancel.requested",
            )
            .llvm_ctx("inspect restart waiter cancellation")?;
        let inspect = self
            .ctx
            .append_basic_block(self.value, "restart.wait.inspect");
        self.builder
            .build_conditional_branch(cancelled_now, cancelled, inspect)
            .llvm_ctx("select restart waiter cancellation")?;
        self.builder.position_at_end(inspect);
        let status = self.state_value("hew_supervisor_native_restart_wait_poll", wait)?;
        let ready = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                status,
                self.ctx.i32_type().const_zero(),
                "restart.wait.ready",
            )
            .llvm_ctx("inspect role restart")?;
        self.builder
            .build_conditional_branch(ready, complete, pending)
            .llvm_ctx("select restart wait readiness")?;
        self.builder.position_at_end(pending);
        frame.suspend(self.ctx, &self.builder, poll, destroyed)?;
        self.builder.position_at_end(destroyed);
        self.reject_invalid_task_state()?;
        self.builder.position_at_end(cancelled);
        self.free_handle("hew_supervisor_native_restart_wait_free", wait)?;
        self.initialize_cancellation_fault()?;
        if let Some(unwind) = unwind {
            self.emit_edge(unwind)?;
        } else {
            self.emit_propagate_fault()?;
        }
        self.builder.position_at_end(complete);
        self.free_handle("hew_supervisor_native_restart_wait_free", wait)
    }

    pub(super) fn new_actor_wait_edge(
        &self,
        target: BasicMetadataValueEnum<'ctx>,
        operation: u64,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("actor wait edge requires an invocation".into())
        })?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let target_data = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target_data, None);
        let new = get_or_declare_external(
            self.llvm,
            "hew_actor_wait_edge_new",
            ptr.fn_type(
                &[ptr.into(), size_ty.into(), self.ctx.i32_type().into()],
                false,
            ),
        )?;
        Ok(call_value(
            &self.builder,
            new,
            &[
                frame.state.into(),
                target,
                self.ctx.i32_type().const_int(operation, false).into(),
            ],
            "actor.wait.edge",
        )?
        .into_pointer_value())
    }

    pub(super) fn check_actor_wait_cycle(
        &self,
        edge: PointerValue<'ctx>,
        cycle: BasicBlock<'ctx>,
    ) -> CodegenResult<()> {
        let status = self.state_value("hew_actor_wait_edge_pending", edge)?;
        let detected = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                status,
                self.ctx.i32_type().const_zero(),
                "actor.wait.cycle",
            )
            .llvm_ctx("inspect local wait cycle")?;
        let pending = self
            .ctx
            .append_basic_block(self.value, "actor.wait.acyclic");
        self.builder
            .build_conditional_branch(detected, cycle, pending)
            .llvm_ctx("route a proven local cycle through cleanup")?;
        self.builder.position_at_end(pending);
        Ok(())
    }

    pub(super) fn initialize_actor_cycle_fault(
        &self,
        edge: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let fault = get_or_declare_external(
            self.llvm,
            "hew_actor_wait_edge_fault",
            ptr.fn_type(&[ptr.into()], false),
        )?;
        let fault = call_value(&self.builder, fault, &[edge.into()], "actor.wait.fault")?;
        self.store_active_fault(fault, HEW_TRAP_USER_PANIC)
    }

    pub(super) fn discard_pending_message(&self, source: StorageId) -> CodegenResult<()> {
        let recipe = self
            .module
            .actor_recipes
            .get(&self.storage(source)?.ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("pending message lacks its value recipe".into())
            })?;
        if let Some(action) = recipe.destroy {
            self.destroy_owned_operand(source, action)
        } else {
            self.clear_owned(source)
        }
    }

    pub(super) fn emit_actor_await_stopped(
        &self,
        source: StorageId,
        unwind: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        self.emit_actor_wait(source, unwind, false)
    }

    pub(super) fn emit_actor_wait(
        &self,
        source: StorageId,
        unwind: Option<&PhysicalEdge>,
        mandatory_cleanup: bool,
    ) -> CodegenResult<()> {
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("actor wait requires a resumable invocation".into())
        })?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let wake = get_or_declare_external(
            self.llvm,
            "hew_coro_state_waker",
            ptr.fn_type(&[ptr.into()], false),
        )?;
        let waker = call_value(
            &self.builder,
            wake,
            &[frame.state.into()],
            "actor.wait.waker",
        )?;
        let target = self.load(source, "actor.wait.target")?;
        let cycle = self
            .ctx
            .append_basic_block(self.value, "actor.wait.cycle.fault");
        let new = get_or_declare_external(
            self.llvm,
            "hew_actor_wait_new",
            ptr.fn_type(&[target.get_type().into(), ptr.into()], false),
        )?;
        let arguments = [target.into(), waker.into()];
        let wait = call_value(&self.builder, new, &arguments, "actor.wait")?.into_pointer_value();
        let edge_new = get_or_declare_external(
            self.llvm,
            "hew_actor_wait_edge_new_for_wait",
            ptr.fn_type(&[ptr.into(), ptr.into(), self.ctx.i32_type().into()], false),
        )?;
        let edge = if mandatory_cleanup {
            ptr.const_null()
        } else {
            call_value(
                &self.builder,
                edge_new,
                &[
                    frame.state.into(),
                    wait.into(),
                    self.ctx.i32_type().const_int(2, false).into(),
                ],
                "actor.wait.edge",
            )?
            .into_pointer_value()
        };
        let poll = self.ctx.append_basic_block(self.value, "actor.wait.poll");
        let inspect = self
            .ctx
            .append_basic_block(self.value, "actor.wait.inspect");
        let pending = self
            .ctx
            .append_basic_block(self.value, "actor.wait.pending");
        let complete = self
            .ctx
            .append_basic_block(self.value, "actor.wait.complete");
        let cancelled = self
            .ctx
            .append_basic_block(self.value, "actor.wait.cancelled");
        let failed = self.ctx.append_basic_block(self.value, "actor.wait.failed");
        let destroyed = self
            .ctx
            .append_basic_block(self.value, "actor.wait.invalid.destroy");
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("poll actor termination")?;
        self.builder.position_at_end(poll);
        self.free_handle("hew_actor_wait_edge_prepare", edge)?;
        let cancellation = if mandatory_cleanup {
            self.ctx.i32_type().const_zero()
        } else {
            self.state_value("hew_coro_state_is_cancelled", frame.state)?
        };
        let cancelled_now = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                cancellation,
                self.ctx.i32_type().const_zero(),
                "actor.wait.cancel.requested",
            )
            .llvm_ctx("inspect waiter cancellation")?;
        self.builder
            .build_conditional_branch(cancelled_now, cancelled, inspect)
            .llvm_ctx("select waiter cancellation")?;
        self.builder.position_at_end(inspect);
        let status = self.state_value("hew_actor_wait_poll", wait)?;
        self.builder
            .build_switch(
                status,
                failed,
                &[
                    (self.ctx.i32_type().const_zero(), pending),
                    (self.ctx.i32_type().const_int(1, false), complete),
                ],
            )
            .llvm_ctx("inspect terminal cleanup")?;
        self.builder.position_at_end(pending);
        if !mandatory_cleanup {
            self.check_actor_wait_cycle(edge, cycle)?;
        }
        frame.suspend(self.ctx, &self.builder, poll, destroyed)?;
        self.builder.position_at_end(destroyed);
        self.reject_invalid_task_state()?;
        // The three failing exits record their fault and share one release.
        let mut join = self.exit_join("actor.wait.release");
        self.builder.position_at_end(failed);
        let take_fault = get_or_declare_external(
            self.llvm,
            "hew_actor_wait_take_fault",
            ptr.fn_type(&[ptr.into()], false),
        )?;
        let fault = call_value(
            &self.builder,
            take_fault,
            &[wait.into()],
            "actor.close.fault",
        )?;
        if mandatory_cleanup {
            let primary = self
                .builder
                .build_load(ptr, self.active_fault, "spawn.original.fault")
                .llvm_ctx("retain the construction fault")?;
            let combine = get_or_declare_external(
                self.llvm,
                "hew_fault_combine",
                ptr.fn_type(&[ptr.into(), ptr.into()], false),
            )?;
            let combined = call_value(
                &self.builder,
                combine,
                &[primary.into(), fault.into()],
                "spawn.combined.fault",
            )?;
            self.builder
                .build_store(self.active_fault, combined)
                .llvm_ctx("append construction cleanup diagnostics")?;
            self.builder
                .build_unconditional_branch(complete)
                .llvm_ctx("finish construction cleanup")?;
        } else {
            let code = self.state_value("hew_fault_code", fault.into_pointer_value())?;
            self.store_active_fault_value(fault, code)?;
            self.leave(&mut join, suspend::EXIT_UNWIND)?;
        }
        self.builder.position_at_end(cancelled);
        self.initialize_cancellation_fault()?;
        self.leave(&mut join, suspend::EXIT_UNWIND)?;
        self.builder.position_at_end(cycle);
        self.initialize_actor_cycle_fault(edge)?;
        self.leave(&mut join, suspend::EXIT_UNWIND)?;
        self.enter_join(&join)?;
        self.free_handle("hew_actor_wait_edge_free", edge)?;
        self.free_handle("hew_actor_wait_free", wait)?;
        if let Some(unwind) = unwind {
            self.emit_edge(unwind)?;
        } else {
            self.emit_propagate_fault()?;
        }
        self.builder.position_at_end(complete);
        self.free_handle("hew_actor_wait_edge_free", edge)?;
        self.free_handle("hew_actor_wait_free", wait)
    }

    #[allow(
        clippy::too_many_arguments,
        reason = "one owned submission and its exact protocol"
    )]
    pub(super) fn emit_owned_actor_submission(
        &self,
        request: &[BasicMetadataValueEnum<'ctx>],
        payload_release: PointerValue<'ctx>,
        source: StorageId,
        unwind: Option<&PhysicalEdge>,
        policy: hew_types::actor_delivery::SendPolicy,
        dispatch: PointerValue<'ctx>,
        owns_target: bool,
    ) -> CodegenResult<(IntValue<'ctx>, IntValue<'ctx>, IntValue<'ctx>)> {
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("submission requires a resumable invocation".into())
        })?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let target = TargetData::create(&self.module.target.data_layout);
        let word = self.ctx.ptr_sized_int_type(&target, None);
        let wake = get_or_declare_external(
            self.llvm,
            "hew_coro_state_waker",
            ptr.fn_type(&[ptr.into()], false),
        )?;
        let waker = call_value(
            &self.builder,
            wake,
            &[frame.state.into()],
            "submission.waker",
        )?;
        let new = get_or_declare_external(
            self.llvm,
            "hew_actor_submit_new",
            ptr.fn_type(
                &[
                    word.into(),
                    self.ctx.i32_type().into(),
                    ptr.into(),
                    word.into(),
                    ptr.into(),
                    ptr.into(),
                    self.ctx.i64_type().into(),
                    self.ctx.i8_type().into(),
                    ptr.into(),
                    ptr.into(),
                    ptr.into(),
                    self.ctx.bool_type().into(),
                ],
                false,
            ),
        )?;
        use hew_runtime::envelope::RequestKind;
        use hew_types::actor_delivery::SendPolicy;
        let kind = match policy {
            SendPolicy::Wait => RequestKind::SendWait,
            SendPolicy::Reject => RequestKind::SendReject,
            SendPolicy::DropNewest => RequestKind::SendDropNewest,
        };
        let mut args = request.to_vec();
        args.extend::<[BasicMetadataValueEnum<'ctx>; 7]>([
            waker.into(),
            self.request_deadline()?.into(),
            self.ctx.i8_type().const_int(kind as u64, false).into(),
            payload_release.into(),
            self.sender_stamp()?.into(),
            dispatch.into(),
            self.ctx
                .bool_type()
                .const_int(u64::from(owns_target), false)
                .into(),
        ]);
        let operation =
            call_value(&self.builder, new, &args, "submission.operation")?.into_pointer_value();
        let missing = self
            .builder
            .build_is_null(operation, "submission.not_sealed")
            .llvm_ctx("check message sealing")?;
        let unavailable = self
            .ctx
            .append_basic_block(self.value, "submission.unavailable");
        let owned = self.ctx.append_basic_block(self.value, "submission.owned");
        let done = self.ctx.append_basic_block(self.value, "submission.ready");
        self.builder
            .build_conditional_branch(missing, unavailable, owned)
            .llvm_ctx("transfer only sealed message fields")?;
        self.builder.position_at_end(unavailable);
        self.builder
            .build_unconditional_branch(done)
            .llvm_ctx("return the unsealed message")?;
        self.builder.position_at_end(owned);
        self.clear_owned(source)?;
        let edge = self.new_actor_wait_edge(request[0], 1)?;
        let cycle = self.ctx.append_basic_block(self.value, "submission.cycle");
        let poll = self.ctx.append_basic_block(self.value, "submission.poll");
        let inspect = self
            .ctx
            .append_basic_block(self.value, "submission.inspect");
        let pending = self
            .ctx
            .append_basic_block(self.value, "submission.pending");
        let complete = self
            .ctx
            .append_basic_block(self.value, "submission.complete");
        let cancelled = self
            .ctx
            .append_basic_block(self.value, "submission.cancelled");
        let destroyed = self
            .ctx
            .append_basic_block(self.value, "submission.invalid");
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("poll owned admission")?;
        self.builder.position_at_end(poll);
        self.free_handle("hew_actor_wait_edge_prepare", edge)?;
        let cancellation = self.state_value("hew_coro_state_is_cancelled", frame.state)?;
        let cancelled_now = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                cancellation,
                self.ctx.i32_type().const_zero(),
                "submission.cancel.requested",
            )
            .llvm_ctx("inspect cancellation")?;
        self.builder
            .build_conditional_branch(cancelled_now, cancelled, inspect)
            .llvm_ctx("route cancellation")?;
        self.builder.position_at_end(inspect);
        let status = self.state_value("hew_actor_call_poll", operation)?;
        let waiting = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_all_ones(),
                "submission.waiting",
            )
            .llvm_ctx("inspect admission readiness")?;
        self.builder
            .build_conditional_branch(waiting, pending, complete)
            .llvm_ctx("route admission readiness")?;
        self.builder.position_at_end(pending);
        self.check_actor_wait_cycle(edge, cycle)?;
        frame.suspend(self.ctx, &self.builder, poll, destroyed)?;
        self.builder.position_at_end(destroyed);
        self.reject_invalid_task_state()?;
        let mut join = self.exit_join("submission.release");
        self.builder.position_at_end(cancelled);
        self.initialize_cancellation_fault()?;
        self.leave(&mut join, suspend::EXIT_UNWIND)?;
        self.builder.position_at_end(cycle);
        self.initialize_actor_cycle_fault(edge)?;
        self.leave(&mut join, suspend::EXIT_UNWIND)?;
        self.enter_join(&join)?;
        self.free_handle("hew_actor_wait_edge_free", edge)?;
        self.release_handle(release::Handle::ActorCall, operation)?;
        if let Some(unwind) = unwind {
            self.emit_edge(unwind)?;
        } else {
            self.emit_propagate_fault()?;
        }
        self.builder.position_at_end(complete);
        let refused = self
            .value_emitter()
            .entry_scratch(self.ctx.bool_type().into(), "submission.refused")?;
        let discarded = self
            .value_emitter()
            .entry_scratch(self.ctx.bool_type().into(), "submission.discarded")?;
        let take = get_or_declare_external(
            self.llvm,
            "hew_actor_submit_take",
            self.ctx.i32_type().fn_type(&[ptr.into(); 3], false),
        )?;
        let status = call_value(
            &self.builder,
            take,
            &[operation.into(), refused.into(), discarded.into()],
            "submission.status",
        )?
        .into_int_value();
        let refused = self
            .builder
            .build_load(self.ctx.bool_type(), refused, "submission.was_refused")
            .llvm_ctx("read refusal evidence")?
            .into_int_value();
        let discarded = self
            .builder
            .build_load(self.ctx.bool_type(), discarded, "submission.was_discarded")
            .llvm_ctx("read discard outcome")?
            .into_int_value();
        self.free_handle("hew_actor_wait_edge_free", edge)?;
        self.release_handle(release::Handle::ActorCall, operation)?;
        let taken = self.builder.get_insert_block().unwrap();
        self.builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish owned admission")?;
        self.builder.position_at_end(done);
        let result = self
            .builder
            .build_phi(self.ctx.i32_type(), "submission.result")
            .llvm_ctx("join submission status")?;
        result.add_incoming(&[
            (
                &self.ctx.i32_type().const_int(
                    hew_runtime::internal::types::AskError::SendFailed as u64,
                    false,
                ),
                unavailable,
            ),
            (&status, taken),
        ]);
        let refusal = self
            .builder
            .build_phi(self.ctx.bool_type(), "submission.refusal")
            .llvm_ctx("join submission refusal")?;
        refusal.add_incoming(&[
            (&self.ctx.bool_type().const_int(1, false), unavailable),
            (&refused, taken),
        ]);
        let dropped = self
            .builder
            .build_phi(self.ctx.bool_type(), "submission.discarded")
            .llvm_ctx("join submission discard")?;
        dropped.add_incoming(&[
            (&self.ctx.bool_type().const_zero(), unavailable),
            (&discarded, taken),
        ]);
        Ok((
            result.as_basic_value().into_int_value(),
            refusal.as_basic_value().into_int_value(),
            dropped.as_basic_value().into_int_value(),
        ))
    }

    pub(super) fn emit_actor_send_wait(
        &self,
        request: &[BasicMetadataValueEnum<'ctx>],
        release: PointerValue<'ctx>,
        source: StorageId,
        unwind: Option<&PhysicalEdge>,
        obligated: bool,
    ) -> CodegenResult<IntValue<'ctx>> {
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("waiting submission needs a resumable invocation".into())
        })?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let target = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let wake = get_or_declare_external(
            self.llvm,
            "hew_coro_state_waker",
            ptr.fn_type(&[ptr.into()], false),
        )?;
        let waker = call_value(&self.builder, wake, &[frame.state.into()], "send.waker")?;
        let new = get_or_declare_external(
            self.llvm,
            "hew_actor_send_wait_new",
            ptr.fn_type(
                &[
                    size_ty.into(),
                    self.ctx.i32_type().into(),
                    ptr.into(),
                    size_ty.into(),
                    ptr.into(),
                    ptr.into(),
                    ptr.into(),
                    self.ctx.i32_type().into(),
                    ptr.into(),
                ],
                false,
            ),
        )?;
        let edge = self.new_actor_wait_edge(request[0], 1)?;
        let cycle = self.ctx.append_basic_block(self.value, "send.cycle.fault");
        let mut args = request.to_vec();
        args.push(waker.into());
        args.push(release.into());
        args.push(
            self.ctx
                .i32_type()
                .const_int(u64::from(obligated), false)
                .into(),
        );
        args.push(self.sender_stamp()?.into());
        let wait = call_value(&self.builder, new, &args, "send.wait")?.into_pointer_value();
        let poll = self.ctx.append_basic_block(self.value, "send.poll");
        let inspect = self.ctx.append_basic_block(self.value, "send.inspect");
        let pending = self.ctx.append_basic_block(self.value, "send.pending");
        let complete = self.ctx.append_basic_block(self.value, "send.complete");
        let cancelled = self.ctx.append_basic_block(self.value, "send.cancelled");
        let destroyed = self
            .ctx
            .append_basic_block(self.value, "send.invalid.destroy");
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("poll waiting send")?;
        self.builder.position_at_end(poll);
        self.free_handle("hew_actor_wait_edge_prepare", edge)?;
        let cancellation = self.state_value("hew_coro_state_is_cancelled", frame.state)?;
        let cancelled_now = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                cancellation,
                self.ctx.i32_type().const_zero(),
                "send.cancel.requested",
            )
            .llvm_ctx("inspect sender cancellation")?;
        self.builder
            .build_conditional_branch(cancelled_now, cancelled, inspect)
            .llvm_ctx("select sender cancellation")?;
        self.builder.position_at_end(inspect);
        let status = self.state_value("hew_actor_send_wait_poll", wait)?;
        let waiting = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                super::actor::submit_status(self.ctx, HewSubmitStatus::Pending),
                "send.waiting",
            )
            .llvm_ctx("inspect capacity readiness")?;
        self.builder
            .build_conditional_branch(waiting, pending, complete)
            .llvm_ctx("select capacity readiness")?;
        self.builder.position_at_end(pending);
        self.check_actor_wait_cycle(edge, cycle)?;
        frame.suspend(self.ctx, &self.builder, poll, destroyed)?;
        self.builder.position_at_end(destroyed);
        self.reject_invalid_task_state()?;
        // Both failing exits record their fault and share one release.
        let mut join = self.exit_join("send.release");
        self.builder.position_at_end(cancelled);
        self.initialize_cancellation_fault()?;
        self.leave(&mut join, suspend::EXIT_UNWIND)?;
        self.builder.position_at_end(cycle);
        self.initialize_actor_cycle_fault(edge)?;
        self.leave(&mut join, suspend::EXIT_UNWIND)?;
        self.enter_join(&join)?;
        self.free_handle("hew_actor_wait_edge_free", edge)?;
        self.free_handle("hew_actor_send_wait_free", wait)?;
        self.discard_pending_message(source)?;
        if let Some(unwind) = unwind {
            self.emit_edge(unwind)?;
        } else {
            self.emit_propagate_fault()?;
        }
        self.builder.position_at_end(complete);
        self.free_handle("hew_actor_send_wait_free", wait)?;
        self.free_handle("hew_actor_wait_edge_free", edge)?;
        Ok(status)
    }
}
