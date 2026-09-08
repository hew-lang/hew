//! Bounded element pipes between a stream producer turn and its consumer.

use super::*;

fn pipe_descriptor_symbol(callable: CallableId, stream: StorageId) -> String {
    format!("__hew_stream_pipe_{}_{}_elem", callable.0, stream.0)
}

fn stream_descriptor_symbol(callable: CallableId, block: BlockId) -> String {
    format!("__hew_stream_{}_{}_elem", callable.0, block.0)
}

impl<'ctx> ModuleEmitter<'ctx, '_> {
    /// Producers, consumers and queued-value cleanup use the same typed witness.
    pub(super) fn emit_stream_descriptors(&self) -> CodegenResult<()> {
        for function in &self.module.functions {
            for block in &function.blocks {
                for operation in &block.ops {
                    if let PhysicalOp::StreamPipe {
                        stream, element, ..
                    } = operation
                    {
                        self.emit_value_descriptor(
                            &pipe_descriptor_symbol(function.callable, *stream),
                            element,
                        )?;
                    }
                }
                if let PhysicalTerminator::StreamNext { element, .. }
                | PhysicalTerminator::StreamSend { element, .. } = &block.terminator
                {
                    self.emit_value_descriptor(
                        &stream_descriptor_symbol(function.callable, block.id),
                        element,
                    )?;
                }
            }
        }
        Ok(())
    }
}

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    pub(super) fn emit_stream_pipe(
        &self,
        capacity: u32,
        stream: StorageId,
        sink: StorageId,
    ) -> CodegenResult<()> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let witness = self
            .llvm
            .get_global(&pipe_descriptor_symbol(self.function.callable, stream))
            .ok_or_else(|| CodegenError::FailClosed("stream pipe witness was not emitted".into()))?
            .as_pointer_value();
        let sink_out = self
            .value_emitter()
            .entry_scratch(pointer.into(), "stream.sink.out")?;
        let make = coro::external(
            self.llvm,
            "hew_stream_pipe_native",
            pointer.fn_type(
                &[self.ctx.i64_type().into(), pointer.into(), pointer.into()],
                false,
            ),
        )?;
        let handle = suspend::call_value(
            &self.builder,
            make,
            &[
                self.ctx
                    .i64_type()
                    .const_int(u64::from(capacity), false)
                    .into(),
                witness.into(),
                sink_out.into(),
            ],
            "stream.pipe",
        )?;
        self.store(stream, handle)?;
        let sink_handle = self
            .builder
            .build_load(pointer, sink_out, "stream.sink")
            .llvm_ctx("load pipe sink half")?;
        self.store(sink, sink_handle)
    }

    fn stream_frame(&self) -> CodegenResult<&coro::Frame<'ctx>> {
        self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("stream operation requires a resumable invocation".into())
        })
    }

    fn stream_cancelled(
        &self,
        frame: &coro::Frame<'ctx>,
        name: &str,
    ) -> CodegenResult<IntValue<'ctx>> {
        let status = self.state_value("hew_coro_state_is_cancelled", frame.state)?;
        self.builder
            .build_int_compare(
                IntPredicate::NE,
                status,
                self.ctx.i32_type().const_zero(),
                name,
            )
            .llvm_ctx("observe stream peer cancellation")
    }

    fn stream_witness(&self, block: BlockId) -> CodegenResult<PointerValue<'ctx>> {
        self.llvm
            .get_global(&stream_descriptor_symbol(self.function.callable, block))
            .map(|global| global.as_pointer_value())
            .ok_or_else(|| {
                CodegenError::FailClosed("stream element witness was not emitted".into())
            })
    }

    fn drain_stream(
        &self,
        request: PointerValue<'ctx>,
        waker: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let frame = self.stream_frame()?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let poll = self.ctx.append_basic_block(self.value, "stream.drain.poll");
        let pending = self
            .ctx
            .append_basic_block(self.value, "stream.drain.pending");
        let drained = self.ctx.append_basic_block(self.value, "stream.drained");
        let destroyed = self
            .ctx
            .append_basic_block(self.value, "stream.drain.invalid");
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("poll stream quiescence")?;
        self.builder.position_at_end(poll);
        let status = coro::external(
            self.llvm,
            "hew_stream_cleanup_status_native",
            self.ctx.i32_type().fn_type(&[pointer.into(); 2], false),
        )?;
        let ready = suspend::call_value(
            &self.builder,
            status,
            &[request.into(), waker.into()],
            "stream.quiescent",
        )?
        .into_int_value();
        let ready = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                ready,
                self.ctx.i32_type().const_zero(),
                "stream.drain.ready",
            )
            .llvm_ctx("check stream quiescence")?;
        self.builder
            .build_conditional_branch(ready, drained, pending)
            .llvm_ctx("wait for stream producer release")?;
        self.builder.position_at_end(pending);
        frame.suspend(self.ctx, self.llvm, &self.builder, poll, destroyed, false)?;
        self.builder.position_at_end(destroyed);
        self.reject_invalid_task_state()?;
        self.builder.position_at_end(drained);
        Ok(())
    }

    fn finish_stream(
        &self,
        request: PointerValue<'ctx>,
        waker: PointerValue<'ctx>,
        cancelled: inkwell::basic_block::BasicBlock<'ctx>,
    ) -> CodegenResult<()> {
        self.drain_stream(request, waker)?;
        let ready = self
            .ctx
            .append_basic_block(self.value, "stream.result.ready");
        let stopping = self.stream_cancelled(self.stream_frame()?, "stream.cancel.after.drain")?;
        self.builder
            .build_conditional_branch(stopping, cancelled, ready)
            .llvm_ctx("admit stream result after drain")?;
        self.builder.position_at_end(ready);
        Ok(())
    }

    pub(super) fn emit_stream_next(&self, block: &PhysicalBlock) -> CodegenResult<()> {
        let PhysicalTerminator::StreamNext {
            stream,
            element,
            result,
            normal,
            cancel,
            unwind,
        } = &block.terminator
        else {
            return Err(CodegenError::FailClosed(
                "stream receive requires its own terminator".into(),
            ));
        };
        let result = *result;
        let witness = self.stream_witness(block.id)?;
        let ArgumentTransfer::BorrowMut(stream) = stream else {
            return Err(CodegenError::FailClosed(
                "stream receive requires an exclusive stream".into(),
            ));
        };
        let frame = self.stream_frame()?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let handle = self.load(*stream, "stream.receiver")?;
        let option = self
            .module
            .variant_glue
            .iter()
            .find(|glue| glue.ty == self.storage(result).unwrap().ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("stream receive lacks its Option recipe".into())
            })?;
        let element =
            self.module.target.layout(&element.ty).ok_or_else(|| {
                CodegenError::FailClosed("stream element lacks its layout".into())
            })?;
        let element_ty = llvm_type(self.ctx, &element.repr)?;
        let slot = self
            .value_emitter()
            .entry_scratch(element_ty, "stream.element.slot")?;
        let waker = self.task_pointer_call("hew_coro_state_waker", &[frame.state.into()])?;
        let start = coro::external(
            self.llvm,
            "hew_stream_read_start_native",
            pointer.fn_type(&[pointer.into(); 3], false),
        )?;
        let request = suspend::call_value(
            &self.builder,
            start,
            &[handle.into(), waker.into(), witness.into()],
            "stream.read.operation",
        )?
        .into_pointer_value();
        let poll = self.ctx.append_basic_block(self.value, "stream.next.poll");
        let inspect = self
            .ctx
            .append_basic_block(self.value, "stream.next.inspect");
        let wait = self.ctx.append_basic_block(self.value, "stream.next.wait");
        let invalid = self
            .ctx
            .append_basic_block(self.value, "stream.next.invalid.destroy");
        let some = self.ctx.append_basic_block(self.value, "stream.next.some");
        let none = self.ctx.append_basic_block(self.value, "stream.next.none");
        let failed = self
            .ctx
            .append_basic_block(self.value, "stream.next.failed");
        let cancelled = self
            .ctx
            .append_basic_block(self.value, "stream.next.cancelled");
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("poll stream")?;
        self.builder.position_at_end(poll);
        let closing = self.stream_cancelled(frame, "stream.consumer.cancelled")?;
        self.builder
            .build_conditional_branch(closing, cancelled, inspect)
            .llvm_ctx("select stream consumer cancellation")?;
        self.builder.position_at_end(inspect);
        let status = self.state_value("hew_stream_read_poll_native", request)?;
        self.builder
            .build_switch(
                status,
                failed,
                &[
                    (self.ctx.i32_type().const_zero(), wait),
                    (self.ctx.i32_type().const_int(1, false), some),
                    (self.ctx.i32_type().const_int(2, false), none),
                ],
            )
            .llvm_ctx("dispatch stream receive outcome")?;
        self.builder.position_at_end(wait);
        frame.suspend(self.ctx, self.llvm, &self.builder, poll, invalid, false)?;
        self.builder.position_at_end(invalid);
        self.reject_invalid_task_state()?;
        self.builder.position_at_end(some);
        self.finish_stream(request, waker, cancelled)?;
        let take = coro::external(
            self.llvm,
            "hew_stream_read_take_native",
            self.ctx.i32_type().fn_type(&[pointer.into(); 2], false),
        )?;
        suspend::call_value(
            &self.builder,
            take,
            &[request.into(), slot.into()],
            "stream.read.taken",
        )?;
        self.free_handle("hew_stream_operation_free_native", request)?;
        let value = self
            .builder
            .build_load(element_ty, slot, "stream.element")
            .llvm_ctx("load transferred element")?;
        self.write_variant_value(self.slots[result.0 as usize], 0, &[value], option.id)?;
        self.set_place_initialized(result, true)?;
        self.emit_edge(normal)?;
        self.builder.position_at_end(none);
        self.finish_stream(request, waker, cancelled)?;
        self.free_handle("hew_stream_operation_free_native", request)?;
        self.write_variant_value(self.slots[result.0 as usize], 1, &[], option.id)?;
        self.set_place_initialized(result, true)?;
        self.emit_edge(normal)?;
        self.builder.position_at_end(cancelled);
        self.free_handle("hew_stream_cancel_native", request)?;
        self.drain_stream(request, waker)?;
        self.free_handle("hew_stream_operation_free_native", request)?;
        self.initialize_cancellation_fault()?;
        self.emit_edge(cancel)?;
        self.builder.position_at_end(failed);
        self.finish_stream(request, waker, cancelled)?;
        self.free_handle("hew_stream_operation_free_native", request)?;
        self.initialize_active_fault(HEW_TRAP_USER_PANIC)?;
        self.emit_edge(unwind)
    }

    pub(super) fn emit_stream_send(&self, block: &PhysicalBlock) -> CodegenResult<()> {
        let PhysicalTerminator::StreamSend {
            sink,
            value,
            normal,
            closed,
            cancel,
            unwind,
            ..
        } = &block.terminator
        else {
            return Err(CodegenError::FailClosed(
                "stream send requires its own terminator".into(),
            ));
        };
        let witness = self.stream_witness(block.id)?;
        let (ArgumentTransfer::Borrow(sink), ArgumentTransfer::Move(value)) = (sink, value) else {
            return Err(CodegenError::FailClosed(
                "stream send borrows its sink and consumes its element".into(),
            ));
        };
        let frame = self.stream_frame()?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let handle = self.load(*sink, "stream.sink")?;
        let waker = self.task_pointer_call("hew_coro_state_waker", &[frame.state.into()])?;
        let start = coro::external(
            self.llvm,
            "hew_stream_write_start_native",
            pointer.fn_type(&[pointer.into(); 4], false),
        )?;
        let request = suspend::call_value(
            &self.builder,
            start,
            &[
                handle.into(),
                waker.into(),
                self.slots[value.0 as usize].into(),
                witness.into(),
            ],
            "stream.write.operation",
        )?
        .into_pointer_value();
        self.clear_owned(*value)?;
        let poll = self.ctx.append_basic_block(self.value, "stream.send.poll");
        let inspect = self
            .ctx
            .append_basic_block(self.value, "stream.send.inspect");
        let wait = self.ctx.append_basic_block(self.value, "stream.send.wait");
        let invalid = self
            .ctx
            .append_basic_block(self.value, "stream.send.invalid.destroy");
        let sent = self.ctx.append_basic_block(self.value, "stream.send.sent");
        let peer_closed = self
            .ctx
            .append_basic_block(self.value, "stream.send.peer.closed");
        let failed = self
            .ctx
            .append_basic_block(self.value, "stream.send.failed");
        let cancelled = self
            .ctx
            .append_basic_block(self.value, "stream.send.cancelled");
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("poll sink capacity")?;
        self.builder.position_at_end(poll);
        let stopping = self.stream_cancelled(frame, "stream.producer.cancelled")?;
        self.builder
            .build_conditional_branch(stopping, cancelled, inspect)
            .llvm_ctx("select stream producer cancellation")?;
        self.builder.position_at_end(inspect);
        let status = self.state_value("hew_stream_write_poll_native", request)?;
        self.builder
            .build_switch(
                status,
                failed,
                &[
                    (self.ctx.i32_type().const_zero(), wait),
                    (self.ctx.i32_type().const_int(1, false), sent),
                    (self.ctx.i32_type().const_int(2, false), peer_closed),
                ],
            )
            .llvm_ctx("dispatch stream send outcome")?;
        self.builder.position_at_end(wait);
        frame.suspend(self.ctx, self.llvm, &self.builder, poll, invalid, false)?;
        self.builder.position_at_end(invalid);
        self.reject_invalid_task_state()?;
        self.builder.position_at_end(sent);
        self.finish_stream(request, waker, cancelled)?;
        self.free_handle("hew_stream_operation_free_native", request)?;
        self.emit_edge(normal)?;
        self.builder.position_at_end(peer_closed);
        self.finish_stream(request, waker, cancelled)?;
        self.free_handle("hew_stream_operation_free_native", request)?;
        self.emit_edge(closed)?;
        self.builder.position_at_end(cancelled);
        self.free_handle("hew_stream_cancel_native", request)?;
        self.drain_stream(request, waker)?;
        self.free_handle("hew_stream_operation_free_native", request)?;
        self.initialize_cancellation_fault()?;
        self.emit_edge(cancel)?;
        self.builder.position_at_end(failed);
        self.finish_stream(request, waker, cancelled)?;
        self.free_handle("hew_stream_operation_free_native", request)?;
        self.initialize_active_fault(HEW_TRAP_USER_PANIC)?;
        self.emit_edge(unwind)
    }
}
