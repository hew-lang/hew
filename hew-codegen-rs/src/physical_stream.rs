//! Bounded element pipes between a stream producer turn and its consumer.

use super::*;

fn element_drop_symbol(callable: CallableId, stream: StorageId) -> String {
    format!("__hew_stream_{}_{}_drop", callable.0, stream.0)
}

impl<'ctx> ModuleEmitter<'ctx, '_> {
    /// Elements still queued when a pipe ends are released by the runtime
    /// through the element's own destroy recipe.
    pub(super) fn emit_stream_descriptors(&self) -> CodegenResult<()> {
        for function in &self.module.functions {
            for operation in function.blocks.iter().flat_map(|block| &block.ops) {
                if let PhysicalOp::StreamPipe {
                    stream, element, ..
                } = operation
                {
                    if let Some(action) = element.destroy {
                        let layout = self.module.target.layout(&element.ty).ok_or_else(|| {
                            CodegenError::FailClosed("stream element lacks its layout".into())
                        })?;
                        self.emit_value_drop_callback(
                            &element_drop_symbol(function.callable, *stream),
                            layout,
                            action,
                        )?;
                    }
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
        element: &PhysicalValueRecipe,
    ) -> CodegenResult<()> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let target = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let layout =
            self.module.target.layout(&element.ty).ok_or_else(|| {
                CodegenError::FailClosed("stream element lacks its layout".into())
            })?;
        let drop = if element.destroy.is_some() {
            self.llvm
                .get_function(&element_drop_symbol(self.function.callable, stream))
                .ok_or_else(|| {
                    CodegenError::FailClosed("stream element drop was not emitted".into())
                })?
                .as_global_value()
                .as_pointer_value()
        } else {
            pointer.const_null()
        };
        let sink_out = self
            .value_emitter()
            .entry_scratch(pointer.into(), "stream.sink.out")?;
        let make = coro::external(
            self.llvm,
            "hew_stream_pipe_native",
            pointer.fn_type(
                &[
                    self.ctx.i64_type().into(),
                    size_ty.into(),
                    pointer.into(),
                    pointer.into(),
                ],
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
                size_ty.const_int(layout.size, false).into(),
                drop.into(),
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

    pub(super) fn emit_stream_next(
        &self,
        stream: &ArgumentTransfer,
        result: StorageId,
        normal: &PhysicalEdge,
        cancel: &PhysicalEdge,
        unwind: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let ArgumentTransfer::BorrowMut(stream) = stream else {
            return Err(CodegenError::FailClosed(
                "stream receive requires an exclusive stream".into(),
            ));
        };
        let frame = self.stream_frame()?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let target = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let handle = self.load(*stream, "stream.receiver")?;
        let option = self
            .module
            .variant_glue
            .iter()
            .find(|glue| glue.ty == self.storage(result).unwrap().ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("stream receive lacks its Option recipe".into())
            })?;
        let element = self
            .module
            .target
            .layout(&option.variants[0].fields[0].ty)
            .ok_or_else(|| CodegenError::FailClosed("stream element lacks its layout".into()))?;
        let element_ty = llvm_type(self.ctx, &element.repr)?;
        let slot = self
            .value_emitter()
            .entry_scratch(element_ty, "stream.element.slot")?;
        let waker = self.task_pointer_call("hew_coro_state_waker", &[frame.state.into()])?;
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
        let next = coro::external(
            self.llvm,
            "hew_stream_next_native",
            self.ctx.i32_type().fn_type(
                &[
                    pointer.into(),
                    pointer.into(),
                    pointer.into(),
                    size_ty.into(),
                ],
                false,
            ),
        )?;
        let status = suspend::call_value(
            &self.builder,
            next,
            &[
                handle.into(),
                waker.into(),
                slot.into(),
                size_ty.const_int(element.size, false).into(),
            ],
            "stream.next.status",
        )?
        .into_int_value();
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
        let value = self
            .builder
            .build_load(element_ty, slot, "stream.element")
            .llvm_ctx("load transferred element")?;
        self.write_variant_value(self.slots[result.0 as usize], 0, &[value], option.id)?;
        self.set_place_initialized(result, true)?;
        self.emit_edge(normal)?;
        self.builder.position_at_end(none);
        self.write_variant_value(self.slots[result.0 as usize], 1, &[], option.id)?;
        self.set_place_initialized(result, true)?;
        self.emit_edge(normal)?;
        self.builder.position_at_end(cancelled);
        self.initialize_cancellation_fault()?;
        self.emit_edge(cancel)?;
        self.builder.position_at_end(failed);
        self.initialize_active_fault(HEW_TRAP_USER_PANIC)?;
        self.emit_edge(unwind)
    }

    #[allow(
        clippy::too_many_arguments,
        reason = "one send owns capacity parking and every element disposition"
    )]
    pub(super) fn emit_stream_send(
        &self,
        sink: &ArgumentTransfer,
        value: &ArgumentTransfer,
        element: &PhysicalValueRecipe,
        normal: &PhysicalEdge,
        closed: &PhysicalEdge,
        cancel: &PhysicalEdge,
        unwind: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let (ArgumentTransfer::Borrow(sink), ArgumentTransfer::Move(value)) = (sink, value) else {
            return Err(CodegenError::FailClosed(
                "stream send borrows its sink and consumes its element".into(),
            ));
        };
        let frame = self.stream_frame()?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let target = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let handle = self.load(*sink, "stream.sink")?;
        let size = self.storage(*value)?.layout.size;
        let waker = self.task_pointer_call("hew_coro_state_waker", &[frame.state.into()])?;
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
        let send = coro::external(
            self.llvm,
            "hew_sink_send_native",
            self.ctx.i32_type().fn_type(
                &[
                    pointer.into(),
                    pointer.into(),
                    pointer.into(),
                    size_ty.into(),
                ],
                false,
            ),
        )?;
        let status = suspend::call_value(
            &self.builder,
            send,
            &[
                handle.into(),
                waker.into(),
                self.slots[value.0 as usize].into(),
                size_ty.const_int(size, false).into(),
            ],
            "stream.send.status",
        )?
        .into_int_value();
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
        self.clear_owned(*value)?;
        self.emit_edge(normal)?;
        let discard = |emitter: &Self| match element.destroy {
            Some(action) => emitter.destroy_value(*value, action),
            None => emitter.clear_owned(*value),
        };
        self.builder.position_at_end(peer_closed);
        discard(self)?;
        self.emit_edge(closed)?;
        self.builder.position_at_end(cancelled);
        discard(self)?;
        self.initialize_cancellation_fault()?;
        self.emit_edge(cancel)?;
        self.builder.position_at_end(failed);
        discard(self)?;
        self.initialize_active_fault(HEW_TRAP_USER_PANIC)?;
        self.emit_edge(unwind)
    }
}
