//! Bounded element pipes between a stream producer turn and its consumer.

use super::*;

fn pipe_descriptor_symbol(callable: CallableId, stream: StorageId) -> String {
    format!("__hew_stream_pipe_{}_{}_elem", callable.0, stream.0)
}

fn stream_descriptor_symbol(callable: CallableId, block: BlockId) -> String {
    format!("__hew_stream_{}_{}_elem", callable.0, block.0)
}

fn codec_descriptor_symbol(callable: CallableId, storage: StorageId) -> String {
    format!("__hew_stream_codec_{}_{}_owner", callable.0, storage.0)
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
                if let PhysicalTerminator::StreamSend { sink, receiver, .. } = &block.terminator {
                    self.emit_value_descriptor(
                        &codec_descriptor_symbol(function.callable, argument_source(sink)),
                        receiver,
                    )?;
                }
                if let PhysicalTerminator::RuntimeCall { action, args, .. } = &block.terminator {
                    if matches!(
                        action.family,
                        RuntimeCallFamily::StreamFromGenerator
                            | RuntimeCallFamily::SinkFromEncoder
                            | RuntimeCallFamily::SinkClone
                    ) {
                        for (argument, recipe) in args.iter().zip(args.value_witnesses()) {
                            self.emit_value_descriptor(
                                &codec_descriptor_symbol(
                                    function.callable,
                                    argument_source(argument),
                                ),
                                recipe,
                            )?;
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    pub(super) fn emit_stream_codec_make(
        &self,
        family: RuntimeCallFamily,
        arguments: &[ArgumentTransfer],
        result: StorageId,
        failure: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        let index = usize::from(family == RuntimeCallFamily::SinkFromEncoder);
        let source = argument_source(&arguments[index]);
        let witness = self
            .llvm
            .get_global(&codec_descriptor_symbol(self.function.callable, source))
            .ok_or_else(|| {
                CodegenError::FailClosed("stream codec owner lacks its value descriptor".into())
            })?
            .as_pointer_value();
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let mut inputs = Vec::new();
        if index == 1 {
            inputs.push(
                self.load(argument_source(&arguments[0]), "codec.sink")?
                    .into(),
            );
            inputs.push(self.slots[source.0 as usize].into());
        } else {
            inputs.push(self.load(source, "codec.generator")?.into());
        }
        if index == 1 {
            let sink_witness = self
                .llvm
                .get_global(&codec_descriptor_symbol(
                    self.function.callable,
                    argument_source(&arguments[0]),
                ))
                .ok_or_else(|| {
                    CodegenError::FailClosed(
                        "framed sink lacks its underlying value descriptor".into(),
                    )
                })?;
            inputs.push(sink_witness.as_pointer_value().into());
        }
        inputs.push(witness.into());
        let checked = matches!(
            family,
            RuntimeCallFamily::SinkClone | RuntimeCallFamily::SinkFromEncoder
        );
        let status = if checked {
            let status = self
                .value_emitter()
                .entry_scratch(self.ctx.i32_type().into(), "sink.status")?;
            inputs.push(status.into());
            Some(status)
        } else {
            None
        };
        let function = get_or_declare_external(
            self.llvm,
            if family == RuntimeCallFamily::SinkClone {
                "hew_sink_clone_checked"
            } else {
                family.c_symbol()
            },
            pointer.fn_type(&vec![pointer.into(); inputs.len()], false),
        )?;
        let value = suspend::call_value(&self.builder, function, &inputs, "stream.codec")?;
        if let Some(status) = status {
            let status = self
                .builder
                .build_load(self.ctx.i32_type(), status, "sink.status")
                .llvm_ctx("read checked Sink construction")?
                .into_int_value();
            self.check_budget_status(
                status,
                failure.ok_or_else(|| {
                    CodegenError::FailClosed("checked Sink construction lacks cleanup".into())
                })?,
            )?;
        }
        for transfer in arguments {
            if let ArgumentTransfer::Move(source) = transfer {
                self.clear_owned(*source)?;
            }
        }
        self.store(result, value)
    }

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
        let make = get_or_declare_external(
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
        let status = get_or_declare_external(
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
        frame.suspend(self.ctx, &self.builder, poll, destroyed)?;
        self.builder.position_at_end(destroyed);
        self.reject_invalid_task_state()?;
        self.builder.position_at_end(drained);
        Ok(())
    }

    /// A failed receive or send traps with the runtime's diagnostic - the
    /// faulted pipe or the I/O error - rather than a bare code.
    fn initialize_stream_fault(&self, request: PointerValue<'ctx>) -> CodegenResult<()> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let fault = get_or_declare_external(
            self.llvm,
            "hew_stream_operation_fault_native",
            pointer.fn_type(&[pointer.into()], false),
        )?;
        let fault = suspend::call_value(&self.builder, fault, &[request.into()], "stream.fault")?;
        let code = self.state_value("hew_fault_code", fault.into_pointer_value())?;
        self.store_active_fault_value(fault, code)
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

    /// `try_recv()` over a stream: one non-blocking take that resumes with
    /// `Some` when an element was written and `None` when the stream is empty
    /// or finished.
    #[allow(
        clippy::too_many_arguments,
        reason = "the take names its handle, slot, element layout, witness and both result halves"
    )]
    fn emit_stream_try_next(
        &self,
        handle: BasicValueEnum<'ctx>,
        slot: PointerValue<'ctx>,
        element: &PhysicalLayout,
        witness: PointerValue<'ctx>,
        result: StorageId,
        option: PhysicalVariantId,
        normal: &PhysicalEdge,
        unwind: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let fault_out = self
            .value_emitter()
            .entry_scratch(pointer.into(), "stream.try_next.fault")?;
        let take = get_or_declare_external(
            self.llvm,
            "hew_stream_try_next_layout",
            self.ctx.i32_type().fn_type(&[pointer.into(); 4], false),
        )?;
        let status = suspend::call_value(
            &self.builder,
            take,
            &[handle.into(), slot.into(), witness.into(), fault_out.into()],
            "stream.try_next.status",
        )?
        .into_int_value();
        let fault = self
            .builder
            .build_load(pointer, fault_out, "stream.try_next.failure")
            .llvm_ctx("read generator poll fault")?
            .into_pointer_value();
        let faulted = self
            .builder
            .build_is_not_null(fault, "stream.try_next.faulted")
            .llvm_ctx("observe generator poll fault")?;
        let failed = self
            .ctx
            .append_basic_block(self.value, "stream.try_next.failed");
        let ready = self
            .ctx
            .append_basic_block(self.value, "stream.try_next.ready");
        self.builder
            .build_conditional_branch(faulted, failed, ready)
            .llvm_ctx("dispatch generator poll fault")?;
        self.builder.position_at_end(failed);
        let code = self.state_value("hew_fault_code", fault)?;
        self.store_active_fault_value(fault.into(), code)?;
        self.emit_edge(unwind)?;
        self.builder.position_at_end(ready);
        let some = self
            .ctx
            .append_basic_block(self.value, "stream.try_next.some");
        let none = self
            .ctx
            .append_basic_block(self.value, "stream.try_next.none");
        let taken = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                status,
                self.ctx.i32_type().const_zero(),
                "stream.try_next.taken",
            )
            .llvm_ctx("observe a non-parking stream take")?;
        self.builder
            .build_conditional_branch(taken, some, none)
            .llvm_ctx("dispatch non-parking stream receive outcome")?;
        self.builder.position_at_end(some);
        let value = self
            .value_emitter()
            .load_value(slot, element, "stream.element")?;
        self.write_variant_value(self.slots[result.0 as usize], 0, &[value], option)?;
        self.set_place_initialized(result, true)?;
        self.emit_edge(normal)?;
        self.builder.position_at_end(none);
        self.write_variant_value(self.slots[result.0 as usize], 1, &[], option)?;
        self.set_place_initialized(result, true)?;
        self.emit_edge(normal)
    }

    pub(super) fn emit_stream_next(&self, block: &PhysicalBlock) -> CodegenResult<()> {
        let PhysicalTerminator::StreamNext {
            park,
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
        let park = *park;
        let witness = self.stream_witness(block.id)?;
        let ArgumentTransfer::BorrowMut(stream) = stream else {
            return Err(CodegenError::FailClosed(
                "stream receive requires an exclusive stream".into(),
            ));
        };
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
        if !park {
            return self.emit_stream_try_next(
                handle, slot, element, witness, result, option.id, normal, unwind,
            );
        }
        let frame = self.stream_frame()?;
        let waker = self.task_pointer_call("hew_coro_state_waker", &[frame.state.into()])?;
        let start = get_or_declare_external(
            self.llvm,
            "hew_stream_read_start_native",
            pointer.fn_type(&[pointer.into(); 4], false),
        )?;
        let request = suspend::call_value(
            &self.builder,
            start,
            &[
                handle.into(),
                waker.into(),
                witness.into(),
                frame.state.into(),
            ],
            "stream.read.operation",
        )?
        .into_pointer_value();
        frame.bind_operation(
            self.ctx,
            &self.builder,
            managed::operation_layout(self.ctx, self.llvm, "hew_managed_visit_stream_operation")?,
            request,
        )?;
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
        frame.suspend(self.ctx, &self.builder, poll, invalid)?;
        self.builder.position_at_end(invalid);
        self.reject_invalid_task_state()?;
        // Every exit converges on one release of the operation; its code
        // then selects the element, the end, the cancel or the unwind edge.
        const SOME: u64 = suspend::EXIT_NORMAL;
        const NONE: u64 = suspend::EXIT_UNWIND + 1;
        let mut join = self.exit_join("stream.next.release");
        self.builder.position_at_end(some);
        self.finish_stream(request, waker, cancelled)?;
        let take = get_or_declare_external(
            self.llvm,
            "hew_stream_read_take_native",
            self.ctx.i32_type().fn_type(&[pointer.into(); 2], false),
        )?;
        let taken = suspend::call_value(
            &self.builder,
            take,
            &[request.into(), slot.into()],
            "stream.read.taken",
        )?
        .into_int_value();
        let accepted = self
            .ctx
            .append_basic_block(self.value, "stream.read.accepted");
        let refused = self
            .ctx
            .append_basic_block(self.value, "stream.read.refused");
        let absent = self
            .ctx
            .append_basic_block(self.value, "stream.read.absent");
        self.builder
            .build_switch(
                taken,
                invalid,
                &[
                    (self.ctx.i32_type().const_int(1, false), accepted),
                    (self.ctx.i32_type().const_int(3, false), refused),
                    (self.ctx.i32_type().const_zero(), absent),
                ],
            )
            .llvm_ctx("publish only an acquired stream read result")?;
        self.builder.position_at_end(refused);
        self.initialize_stream_fault(request)?;
        self.leave(&mut join, suspend::EXIT_UNWIND)?;
        self.builder.position_at_end(absent);
        self.leave(&mut join, NONE)?;
        self.builder.position_at_end(accepted);
        self.leave(&mut join, SOME)?;
        self.builder.position_at_end(none);
        self.finish_stream(request, waker, cancelled)?;
        self.leave(&mut join, NONE)?;
        self.builder.position_at_end(cancelled);
        self.free_handle("hew_stream_cancel_native", request)?;
        self.drain_stream(request, waker)?;
        self.initialize_cancellation_fault()?;
        self.leave(&mut join, suspend::EXIT_CANCEL)?;
        self.builder.position_at_end(failed);
        self.finish_stream(request, waker, cancelled)?;
        self.initialize_stream_fault(request)?;
        self.leave(&mut join, suspend::EXIT_UNWIND)?;
        let selected = self.enter_join(&join)?;
        self.release_stream_operation(request)?;
        let ([some_block, none_block, cancel_block], unwind_block) = self.dispatch_exits(
            selected,
            [
                (SOME, "stream.next.element"),
                (NONE, "stream.next.end"),
                (suspend::EXIT_CANCEL, "stream.next.cancel"),
            ],
            "stream.next.unwind",
        )?;
        self.builder.position_at_end(some_block);
        let value = self
            .value_emitter()
            .load_value(slot, element, "stream.element")?;
        self.write_variant_value(self.slots[result.0 as usize], 0, &[value], option.id)?;
        self.set_place_initialized(result, true)?;
        self.emit_edge(normal)?;
        self.builder.position_at_end(none_block);
        self.write_variant_value(self.slots[result.0 as usize], 1, &[], option.id)?;
        self.set_place_initialized(result, true)?;
        self.emit_edge(normal)?;
        self.builder.position_at_end(cancel_block);
        self.emit_edge(cancel)?;
        self.builder.position_at_end(unwind_block);
        self.emit_edge(unwind)
    }

    pub(super) fn emit_stream_send(&self, block: &PhysicalBlock) -> CodegenResult<()> {
        let PhysicalTerminator::StreamSend {
            park,
            sink,
            value,
            normal,
            closed,
            refused,
            committed,
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
        let handle = self.load(*sink, "stream.sink")?;
        if *park != committed.is_some() {
            return Err(CodegenError::FailClosed(
                "stream send committed slot disagrees with admission".into(),
            ));
        }
        let frame = self.stream_frame()?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let waker = self.task_pointer_call("hew_coro_state_waker", &[frame.state.into()])?;
        let quota_status = frame.storage(
            self.ctx,
            self.ctx.i32_type().into(),
            "stream.write.quota.status",
        )?;
        self.builder
            .build_store(quota_status, self.ctx.i32_type().const_zero())
            .llvm_ctx("initialize checked stream envelope")?;
        let start = get_or_declare_external(
            self.llvm,
            "hew_stream_write_start_native",
            pointer.fn_type(
                &[
                    pointer.into(),
                    pointer.into(),
                    pointer.into(),
                    pointer.into(),
                    pointer.into(),
                    self.ctx.bool_type().into(),
                    pointer.into(),
                    pointer.into(),
                ],
                false,
            ),
        )?;
        let request = suspend::call_value(
            &self.builder,
            start,
            &[
                handle.into(),
                waker.into(),
                self.slots[value.0 as usize].into(),
                witness.into(),
                frame.state.into(),
                self.ctx
                    .bool_type()
                    .const_int(u64::from(*park), false)
                    .into(),
                self.llvm
                    .get_global(&codec_descriptor_symbol(self.function.callable, *sink))
                    .ok_or_else(|| {
                        CodegenError::FailClosed("stream sender lacks its owner descriptor".into())
                    })?
                    .as_pointer_value()
                    .into(),
                quota_status.into(),
            ],
            "stream.write.operation",
        )?
        .into_pointer_value();
        let quota = self
            .builder
            .build_load(
                self.ctx.i32_type(),
                quota_status,
                "stream.write.quota.status",
            )
            .llvm_ctx("read stream envelope reservation")?
            .into_int_value();
        let sealed = self
            .ctx
            .append_basic_block(self.value, "stream.write.sealed");
        let quota_refused = self
            .ctx
            .append_basic_block(self.value, "stream.write.quota.refused");
        let ok = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                quota,
                self.ctx.i32_type().const_zero(),
                "stream.write.quota.ok",
            )
            .llvm_ctx("check stream envelope acquisition")?;
        self.builder
            .build_conditional_branch(ok, sealed, quota_refused)
            .llvm_ctx("retain stream input until checked acquisition")?;
        self.builder.position_at_end(quota_refused);
        self.initialize_active_fault(hew_runtime::internal::types::HEW_TRAP_HEAP_EXCEEDED)?;
        let recipe = &self
            .module
            .managed_types
            .get(&self.storage(*value)?.ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("stream input lacks canonical value recipe".into())
            })?
            .recipe;
        if let Some(action) = recipe.destroy {
            self.destroy_owned_operand(*value, action)?;
        } else {
            self.clear_owned(*value)?;
        }
        self.emit_edge(unwind)?;
        self.builder.position_at_end(sealed);
        frame.bind_operation(
            self.ctx,
            &self.builder,
            managed::operation_layout(self.ctx, self.llvm, "hew_managed_visit_stream_operation")?,
            request,
        )?;
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
        let timed_out = self
            .ctx
            .append_basic_block(self.value, "stream.send.timed.out");
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
                    (self.ctx.i32_type().const_int(4, false), timed_out),
                ],
            )
            .llvm_ctx("dispatch stream send outcome")?;
        self.builder.position_at_end(wait);
        frame.suspend(self.ctx, &self.builder, poll, invalid)?;
        self.builder.position_at_end(invalid);
        self.reject_invalid_task_state()?;
        // Every exit converges on one release; peer closure reads the release
        // outcome only after it.
        const SENT: u64 = suspend::EXIT_NORMAL;
        const CLOSED: u64 = suspend::EXIT_UNWIND + 1;
        const TIMED_OUT: u64 = suspend::EXIT_UNWIND + 2;
        let mut join = self.exit_join("stream.send.release");
        self.builder.position_at_end(sent);
        self.finish_stream(request, waker, cancelled)?;
        self.leave(&mut join, SENT)?;
        self.builder.position_at_end(peer_closed);
        self.finish_stream(request, waker, cancelled)?;
        self.leave(&mut join, CLOSED)?;
        // The deadline's committed count lives in the operation; read it
        // before the operation is released.
        self.builder.position_at_end(timed_out);
        if let Some(committed) = committed {
            let count = suspend::call_value(
                &self.builder,
                get_or_declare_external(
                    self.llvm,
                    "hew_stream_write_committed_native",
                    self.ctx.i64_type().fn_type(&[pointer.into()], false),
                )?,
                &[request.into()],
                "stream.send.committed",
            )?;
            self.store(*committed, count)?;
        }
        self.finish_stream(request, waker, cancelled)?;
        self.leave(&mut join, TIMED_OUT)?;
        self.builder.position_at_end(cancelled);
        self.free_handle("hew_stream_cancel_native", request)?;
        self.drain_stream(request, waker)?;
        self.initialize_cancellation_fault()?;
        self.leave(&mut join, suspend::EXIT_CANCEL)?;
        self.builder.position_at_end(failed);
        self.finish_stream(request, waker, cancelled)?;
        self.initialize_stream_fault(request)?;
        self.leave(&mut join, suspend::EXIT_UNWIND)?;
        let selected = self.enter_join(&join)?;
        self.release_stream_operation(request)?;
        let ([sent_block, closed_block, timed_out_block, cancel_block], unwind_block) = self
            .dispatch_exits(
                selected,
                [
                    (SENT, "stream.send.accepted"),
                    (CLOSED, "stream.send.rejected"),
                    (TIMED_OUT, "stream.send.deadline"),
                    (suspend::EXIT_CANCEL, "stream.send.cancel"),
                ],
                "stream.send.unwind",
            )?;
        self.builder.position_at_end(sent_block);
        self.emit_edge(normal)?;
        self.builder.position_at_end(closed_block);
        let status = self
            .builder
            .build_load(
                self.ctx.i32_type(),
                self.active_status,
                "stream.close.status",
            )
            .llvm_ctx("read rejected element release outcome")?
            .into_int_value();
        self.emit_call_outcome(status, None, Some(closed), Some(unwind))?;
        self.builder.position_at_end(timed_out_block);
        let status = self
            .builder
            .build_load(
                self.ctx.i32_type(),
                self.active_status,
                "stream.deadline.status",
            )
            .llvm_ctx("read timed-out element release outcome")?
            .into_int_value();
        self.emit_call_outcome(status, None, Some(refused), Some(unwind))?;
        self.builder.position_at_end(cancel_block);
        self.emit_edge(cancel)?;
        self.builder.position_at_end(unwind_block);
        self.emit_edge(unwind)
    }
}
