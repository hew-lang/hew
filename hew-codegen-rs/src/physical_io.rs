//! Native requests suspend their caller and release socket loans at quiescence.

use super::*;
use hew_types::runtime_call::{AsyncIoLoan, AsyncIoOp, AsyncIoResume};

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    fn submit_native_io(
        &self,
        operation: AsyncIoOp,
        args: &[ArgumentTransfer],
        waker: PointerValue<'ctx>,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let mut arguments = Vec::<BasicMetadataValueEnum<'ctx>>::new();
        for (index, argument) in args.iter().enumerate() {
            let ArgumentTransfer::Borrow(source) = argument else {
                return Err(CodegenError::FailClosed(
                    "native I/O input must be borrowed".into(),
                ));
            };
            if operation == AsyncIoOp::FileWriteBytes && index == 1 {
                arguments.push(self.slots[source.0 as usize].into());
            } else {
                arguments.push(self.load(*source, "io.input")?.into());
            }
        }
        arguments.push(waker.into());
        let types = arguments
            .iter()
            .map(|argument| match argument {
                BasicMetadataValueEnum::PointerValue(value) => value.get_type().into(),
                BasicMetadataValueEnum::IntValue(value) => value.get_type().into(),
                _ => unreachable!("verified native I/O carrier"),
            })
            .collect::<Vec<_>>();
        let submit = coro::external(
            self.llvm,
            operation.submit_symbol(),
            pointer.fn_type(&types, false),
        )?;
        Ok(
            suspend::call_value(&self.builder, submit, &arguments, "io.request")?
                .into_pointer_value(),
        )
    }

    /// A cancelled readiness operation still owns a resource loan until its
    /// producer has detached. The cleanup wake has its own atomic registration.
    fn drain_native_io(
        &self,
        operation: AsyncIoOp,
        request: PointerValue<'ctx>,
        waker: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        if operation.argument_loan() == AsyncIoLoan::UntilSubmitReturns {
            return Ok(());
        }
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("native I/O requires a resumable body".into())
        })?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let poll = self.ctx.append_basic_block(self.value, "io.drain.poll");
        let pending = self.ctx.append_basic_block(self.value, "io.drain.pending");
        let drained = self.ctx.append_basic_block(self.value, "io.drained");
        let destroyed = self
            .ctx
            .append_basic_block(self.value, "io.drain.invalid.destroy");
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("poll I/O quiescence")?;
        self.builder.position_at_end(poll);
        let status = coro::external(
            self.llvm,
            "hew_async_io_cleanup_status",
            self.ctx.i32_type().fn_type(&[pointer.into(); 2], false),
        )?;
        let ready = suspend::call_value(
            &self.builder,
            status,
            &[request.into(), waker.into()],
            "io.quiescent",
        )?
        .into_int_value();
        let ready = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                ready,
                self.ctx.i32_type().const_zero(),
                "io.drain.ready",
            )
            .llvm_ctx("check I/O quiescence")?;
        self.builder
            .build_conditional_branch(ready, drained, pending)
            .llvm_ctx("wait for I/O producer release")?;
        self.builder.position_at_end(pending);
        frame.suspend(self.ctx, self.llvm, &self.builder, poll, destroyed, false)?;
        self.builder.position_at_end(destroyed);
        self.reject_invalid_task_state()?;
        self.builder.position_at_end(drained);
        Ok(())
    }

    fn take_native_io(
        &self,
        operation: AsyncIoOp,
        request: PointerValue<'ctx>,
        result: StorageId,
    ) -> CodegenResult<()> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let (symbol, output) = match operation.resume() {
            AsyncIoResume::Bytes => ("hew_async_io_take_bytes", self.slots[result.0 as usize]),
            AsyncIoResume::WriteStatus => {
                // The ordinary wrapper returns status rather than the byte count.
                // Freeing the request discards its successful count.
                return self.store(result, self.ctx.i32_type().const_zero().into());
            }
            AsyncIoResume::Connection => {
                let output = self
                    .builder
                    .build_alloca(self.ctx.i64_type(), "io.accepted.handle")
                    .llvm_ctx("allocate accepted handle output")?;
                ("hew_async_io_take_handle", output)
            }
        };
        let take = coro::external(
            self.llvm,
            symbol,
            self.ctx.i32_type().fn_type(&[pointer.into(); 2], false),
        )?;
        let status = suspend::call_value(
            &self.builder,
            take,
            &[request.into(), output.into()],
            "io.taken",
        )?
        .into_int_value();
        let valid = self.ctx.append_basic_block(self.value, "io.take.valid");
        let invalid = self.ctx.append_basic_block(self.value, "io.take.invalid");
        let ok = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_int(1, false),
                "io.take.ok",
            )
            .llvm_ctx("check exact I/O result transfer")?;
        self.builder
            .build_conditional_branch(ok, valid, invalid)
            .llvm_ctx("validate I/O take")?;
        self.builder.position_at_end(invalid);
        self.reject_invalid_task_state()?;
        self.builder.position_at_end(valid);
        if operation.resume() == AsyncIoResume::Connection {
            let handle = self
                .builder
                .build_load(self.ctx.i64_type(), output, "io.accepted")
                .llvm_ctx("load accepted handle")?
                .into_int_value();
            let ty = llvm_type(self.ctx, &self.storage(result)?.layout.repr)?.into_int_type();
            let handle = self
                .builder
                .build_int_cast_sign_flag(handle, ty, true, "io.connection")
                .llvm_ctx("convert accepted handle carrier")?;
            self.store(result, handle.into())?;
        }
        Ok(())
    }

    #[allow(
        clippy::too_many_lines,
        reason = "one request owns readiness, cancellation, quiescence and result transfer"
    )]
    pub(super) fn emit_native_io(
        &self,
        operation: AsyncIoOp,
        args: &[ArgumentTransfer],
        result: StorageId,
        normal: &PhysicalEdge,
        cancel: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("native I/O requires a resumable body".into())
        })?;
        let waker = self.task_pointer_call("hew_coro_state_waker", &[frame.state.into()])?;
        let request = self.submit_native_io(operation, args, waker)?;
        let poll = self.ctx.append_basic_block(self.value, "io.poll");
        let inspect = self.ctx.append_basic_block(self.value, "io.inspect");
        let pending = self.ctx.append_basic_block(self.value, "io.pending");
        let cancelled = self.ctx.append_basic_block(self.value, "io.cancelled");
        let completed = self.ctx.append_basic_block(self.value, "io.completed");
        let success = self.ctx.append_basic_block(self.value, "io.success");
        let error = self.ctx.append_basic_block(self.value, "io.error");
        let resume = self.ctx.append_basic_block(self.value, "io.resume");
        let invalid = self.ctx.append_basic_block(self.value, "io.invalid");
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("poll native I/O")?;
        self.builder.position_at_end(poll);
        let cancellation = self.state_value("hew_coro_state_is_cancelled", frame.state)?;
        let cancellation = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                cancellation,
                self.ctx.i32_type().const_zero(),
                "io.cancel.requested",
            )
            .llvm_ctx("check I/O cancellation")?;
        self.builder
            .build_conditional_branch(cancellation, cancelled, inspect)
            .llvm_ctx("select I/O cancellation")?;
        self.builder.position_at_end(inspect);
        let status = self.state_value("hew_async_io_status", request)?;
        self.builder
            .build_switch(
                status,
                invalid,
                &[
                    (self.ctx.i32_type().const_zero(), pending),
                    (self.ctx.i32_type().const_int(1, false), completed),
                    (self.ctx.i32_type().const_int(2, false), completed),
                ],
            )
            .llvm_ctx("select I/O readiness")?;
        self.builder.position_at_end(pending);
        frame.suspend(self.ctx, self.llvm, &self.builder, poll, invalid, false)?;
        self.builder.position_at_end(invalid);
        self.reject_invalid_task_state()?;
        self.builder.position_at_end(cancelled);
        self.state_value("hew_async_io_cancel", request)?;
        self.drain_native_io(operation, request, waker)?;
        self.free_handle("hew_async_io_free", request)?;
        self.initialize_cancellation_fault()?;
        self.emit_edge(cancel)?;
        self.builder.position_at_end(completed);
        self.drain_native_io(operation, request, waker)?;
        // Quiescence can itself suspend after readiness. Cancellation during
        // that wait abandons the untaken value before ordinary source resumes.
        let ready = self.ctx.append_basic_block(self.value, "io.result.ready");
        let cancellation = self.state_value("hew_coro_state_is_cancelled", frame.state)?;
        let cancellation = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                cancellation,
                self.ctx.i32_type().const_zero(),
                "io.cancel.after.drain",
            )
            .llvm_ctx("check cancellation after I/O drain")?;
        self.builder
            .build_conditional_branch(cancellation, cancelled, ready)
            .llvm_ctx("choose I/O result admission")?;
        self.builder.position_at_end(ready);
        // Error metadata must be restored on the resume thread after all waits.
        // A take changes the request state, so restore before consuming its value.
        let status = self.state_value("hew_async_io_restore_error", request)?;
        let ok = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_int(1, false),
                "io.succeeded",
            )
            .llvm_ctx("test completed I/O outcome")?;
        self.builder
            .build_conditional_branch(ok, success, error)
            .llvm_ctx("choose I/O wrapper result")?;
        self.builder.position_at_end(success);
        self.take_native_io(operation, request, result)?;
        self.builder
            .build_unconditional_branch(resume)
            .llvm_ctx("resume successful I/O")?;
        self.builder.position_at_end(error);
        let result_ty = llvm_type(self.ctx, &self.storage(result)?.layout.repr)?;
        let failed = match operation.resume() {
            AsyncIoResume::Bytes => result_ty.const_zero(),
            AsyncIoResume::WriteStatus | AsyncIoResume::Connection => {
                result_ty.into_int_type().const_all_ones().into()
            }
        };
        self.store(result, failed)?;
        self.builder
            .build_unconditional_branch(resume)
            .llvm_ctx("resume ordinary I/O error")?;
        self.builder.position_at_end(resume);
        self.free_handle("hew_async_io_free", request)?;
        self.emit_result_edge(Some(result), normal)
    }
}
