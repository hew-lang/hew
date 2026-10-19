//! Owned semaphore waits use the shared permit operation and canonical cancellation.

use super::suspend::{call_value, EXIT_CANCEL, EXIT_NORMAL, EXIT_UNWIND};
use super::*;

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    pub(super) fn emit_semaphore_acquire(
        &self,
        operation: hew_types::runtime_call::SemaphoreOp,
        args: &[ArgumentTransfer],
        result: StorageId,
        normal: &PhysicalEdge,
        cancel: &PhysicalEdge,
        unwind: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("semaphore requires a resumable body".into())
        })?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let waker_fn = get_or_declare_external(
            self.llvm,
            "hew_coro_state_waker",
            pointer.fn_type(&[pointer.into()], false),
        )?;
        let waker = call_value(
            &self.builder,
            waker_fn,
            &[frame.state.into()],
            "semaphore.waker",
        )?;
        let start = get_or_declare_external(
            self.llvm,
            "hew_semaphore_acquire_new",
            pointer.fn_type(
                &[pointer.into(), self.ctx.i64_type().into(), pointer.into()],
                false,
            ),
        )?;
        let source = |index: usize| match args.get(index) {
            Some(ArgumentTransfer::Borrow(id)) => Ok(*id),
            _ => Err(CodegenError::FailClosed(
                "semaphore acquisition requires borrowed inputs".into(),
            )),
        };
        let handle = self.load(source(0)?, "semaphore.handle")?;
        let timeout = match operation {
            hew_types::runtime_call::SemaphoreOp::Acquire => self.ctx.i64_type().const_all_ones(),
            hew_types::runtime_call::SemaphoreOp::AcquireTimeout => {
                self.load(source(1)?, "semaphore.timeout")?.into_int_value()
            }
        };
        let operation_kind_is_timed =
            operation == hew_types::runtime_call::SemaphoreOp::AcquireTimeout;
        let operation = call_value(
            &self.builder,
            start,
            &[handle.into(), timeout.into(), waker.into()],
            "semaphore.operation",
        )?
        .into_pointer_value();
        let poll = self.ctx.append_basic_block(self.value, "semaphore.poll");
        let poll_status = self
            .ctx
            .append_basic_block(self.value, "semaphore.poll.status");
        let waiting = self.ctx.append_basic_block(self.value, "semaphore.pending");
        let cancelled = self
            .ctx
            .append_basic_block(self.value, "semaphore.cancelled");
        let destroyed = self
            .ctx
            .append_basic_block(self.value, "semaphore.destroyed");
        let completed = self
            .ctx
            .append_basic_block(self.value, "semaphore.completed");
        let failed = self.ctx.append_basic_block(self.value, "semaphore.failed");
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("poll semaphore")?;
        self.builder.position_at_end(poll);
        let cancellation = self.state_value("hew_coro_state_is_cancelled", frame.state)?;
        let cancellation = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                cancellation,
                self.ctx.i32_type().const_zero(),
                "semaphore.cancel.requested",
            )
            .llvm_ctx("test semaphore cancellation")?;
        self.builder
            .build_conditional_branch(cancellation, cancelled, poll_status)
            .llvm_ctx("select semaphore cancellation")?;
        self.builder.position_at_end(poll_status);
        let status = self.state_value("hew_semaphore_acquire_status", operation)?;
        self.builder
            .build_switch(
                status,
                failed,
                &[
                    (self.ctx.i32_type().const_zero(), waiting),
                    (self.ctx.i32_type().const_int(1, false), completed),
                    (self.ctx.i32_type().const_int(2, false), completed),
                ],
            )
            .llvm_ctx("select semaphore outcome")?;
        self.builder.position_at_end(waiting);
        frame.suspend(self.ctx, &self.builder, poll, destroyed)?;
        self.builder.position_at_end(destroyed);
        self.builder
            .build_unconditional_branch(cancelled)
            .llvm_ctx("run semaphore cancellation cleanup")?;
        let mut join = self.exit_join("semaphore.release");
        self.builder.position_at_end(completed);
        let acquired = self.state_value("hew_semaphore_acquire_take", operation)?;
        if operation_kind_is_timed {
            let acquired = self
                .builder
                .build_int_compare(
                    IntPredicate::NE,
                    acquired,
                    self.ctx.i32_type().const_zero(),
                    "semaphore.acquired",
                )
                .llvm_ctx("read semaphore acquisition result")?;
            let bool_ty = llvm_type(self.ctx, &self.storage(result)?.layout.repr)?.into_int_type();
            let acquired = self
                .builder
                .build_int_z_extend(acquired, bool_ty, "semaphore.acquired.value")
                .llvm_ctx("store semaphore acquisition result")?;
            self.store(result, acquired.into())?;
        } else {
            let ty = llvm_type(self.ctx, &self.storage(result)?.layout.repr)?;
            self.store(result, ty.const_zero())?;
        }
        self.leave(&mut join, EXIT_NORMAL)?;
        self.builder.position_at_end(cancelled);
        self.leave(&mut join, EXIT_CANCEL)?;
        self.builder.position_at_end(failed);
        self.leave(&mut join, EXIT_UNWIND)?;
        let selected = self.enter_join(&join)?;
        self.free_handle("hew_semaphore_acquire_free", operation)?;
        let ([normal_block, cancel_block], unwind_block) = self.dispatch_exits(
            selected,
            [
                (EXIT_NORMAL, "semaphore.normal"),
                (EXIT_CANCEL, "semaphore.cancel"),
            ],
            "semaphore.unwind",
        )?;
        self.builder.position_at_end(normal_block);
        self.emit_result_edge(Some(result), normal)?;
        self.builder.position_at_end(cancel_block);
        self.initialize_cancellation_fault()?;
        self.emit_edge(cancel)?;
        self.builder.position_at_end(unwind_block);
        self.initialize_active_fault(HEW_TRAP_USER_PANIC)?;
        self.emit_edge(unwind)?;
        Ok(())
    }
}
