//! Bounded element queues between channel producers and their consumer.
//!
//! A channel envelope is opaque bytes; the element's typed encode and decode
//! run from the same `HewValueLayout` witness the vector substrate uses, so a
//! string or a heap-owning record crosses the queue through its own clone and
//! drop recipe rather than a slot-width memcpy.

use super::*;

fn channel_descriptor_symbol(callable: CallableId, block: BlockId) -> String {
    format!("__hew_channel_{}_{}_elem", callable.0, block.0)
}

impl<'ctx> ModuleEmitter<'ctx, '_> {
    /// One element witness per channel suspension site.
    pub(super) fn emit_channel_descriptors(&self) -> CodegenResult<()> {
        for function in &self.module.functions {
            for block in &function.blocks {
                let element = match &block.terminator {
                    PhysicalTerminator::ChannelRecv { element, .. }
                    | PhysicalTerminator::ChannelSend { element, .. } => element,
                    _ => continue,
                };
                self.emit_value_descriptor(
                    &channel_descriptor_symbol(function.callable, block.id),
                    element,
                )?;
            }
        }
        Ok(())
    }
}

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    fn channel_frame(&self) -> CodegenResult<&coro::Frame<'ctx>> {
        self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("a channel operation requires a resumable invocation".into())
        })
    }

    fn channel_cancelled(
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
            .llvm_ctx("observe channel peer cancellation")
    }

    fn channel_witness(&self, block: BlockId) -> CodegenResult<PointerValue<'ctx>> {
        let symbol = channel_descriptor_symbol(self.function.callable, block);
        self.llvm
            .get_global(&symbol)
            .map(|global| global.as_pointer_value())
            .ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "channel element witness `{symbol}` was not emitted"
                ))
            })
    }

    pub(super) fn emit_channel_recv(&self, block: &PhysicalBlock) -> CodegenResult<()> {
        let PhysicalTerminator::ChannelRecv {
            channel,
            element,
            result,
            normal,
            cancel,
            unwind,
        } = &block.terminator
        else {
            return Err(CodegenError::FailClosed(
                "channel receive emission requires its own terminator".into(),
            ));
        };
        let result = *result;
        let block = block.id;
        let ArgumentTransfer::BorrowMut(channel) = channel else {
            return Err(CodegenError::FailClosed(
                "channel receive requires an exclusive receiver".into(),
            ));
        };
        let frame = self.channel_frame()?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let handle = self.load(*channel, "channel.receiver")?;
        let witness = self.channel_witness(block)?;
        let option = self
            .module
            .variant_glue
            .iter()
            .find(|glue| glue.ty == self.storage(result).unwrap().ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("channel receive lacks its Option recipe".into())
            })?;
        let layout =
            self.module.target.layout(&element.ty).ok_or_else(|| {
                CodegenError::FailClosed("channel element lacks its layout".into())
            })?;
        let element_ty = llvm_type(self.ctx, &layout.repr)?;
        let slot = self
            .value_emitter()
            .entry_scratch(element_ty, "channel.element.slot")?;
        let waker = self.task_pointer_call("hew_coro_state_waker", &[frame.state.into()])?;
        let poll = self.ctx.append_basic_block(self.value, "channel.recv.poll");
        let inspect = self
            .ctx
            .append_basic_block(self.value, "channel.recv.inspect");
        let cancelled = self
            .ctx
            .append_basic_block(self.value, "channel.recv.cancelled");
        let wait = self.ctx.append_basic_block(self.value, "channel.recv.wait");
        let invalid = self
            .ctx
            .append_basic_block(self.value, "channel.recv.invalid.destroy");
        let some = self.ctx.append_basic_block(self.value, "channel.recv.some");
        let none = self.ctx.append_basic_block(self.value, "channel.recv.none");
        let failed = self
            .ctx
            .append_basic_block(self.value, "channel.recv.failed");
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("poll channel")?;
        self.builder.position_at_end(poll);
        let closing = self.channel_cancelled(frame, "channel.consumer.cancelled")?;
        self.builder
            .build_conditional_branch(closing, cancelled, inspect)
            .llvm_ctx("select channel consumer cancellation")?;
        self.builder.position_at_end(inspect);
        let next = coro::external(
            self.llvm,
            "hew_channel_recv_native",
            self.ctx.i32_type().fn_type(
                &[
                    pointer.into(),
                    pointer.into(),
                    pointer.into(),
                    pointer.into(),
                ],
                false,
            ),
        )?;
        let status = suspend::call_value(
            &self.builder,
            next,
            &[handle.into(), waker.into(), slot.into(), witness.into()],
            "channel.recv.status",
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
            .llvm_ctx("dispatch channel receive outcome")?;
        self.builder.position_at_end(wait);
        frame.suspend(self.ctx, self.llvm, &self.builder, poll, invalid, false)?;
        self.builder.position_at_end(invalid);
        self.reject_invalid_task_state()?;
        self.builder.position_at_end(some);
        let value = self
            .builder
            .build_load(element_ty, slot, "channel.element")
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

    pub(super) fn emit_channel_send(&self, block: &PhysicalBlock) -> CodegenResult<()> {
        let PhysicalTerminator::ChannelSend {
            channel,
            value,
            normal,
            cancel,
            unwind,
            ..
        } = &block.terminator
        else {
            return Err(CodegenError::FailClosed(
                "channel send emission requires its own terminator".into(),
            ));
        };
        let block = block.id;
        let (ArgumentTransfer::BorrowMut(channel), ArgumentTransfer::Borrow(value)) =
            (channel, value)
        else {
            return Err(CodegenError::FailClosed(
                "channel send borrows its sender exclusively and reads its element".into(),
            ));
        };
        let frame = self.channel_frame()?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let handle = self.load(*channel, "channel.sender")?;
        let witness = self.channel_witness(block)?;
        let waker = self.task_pointer_call("hew_coro_state_waker", &[frame.state.into()])?;
        let poll = self.ctx.append_basic_block(self.value, "channel.send.poll");
        let inspect = self
            .ctx
            .append_basic_block(self.value, "channel.send.inspect");
        let cancelled = self
            .ctx
            .append_basic_block(self.value, "channel.send.cancelled");
        let wait = self.ctx.append_basic_block(self.value, "channel.send.wait");
        let invalid = self
            .ctx
            .append_basic_block(self.value, "channel.send.invalid.destroy");
        let sent = self.ctx.append_basic_block(self.value, "channel.send.sent");
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("poll channel capacity")?;
        self.builder.position_at_end(poll);
        let stopping = self.channel_cancelled(frame, "channel.producer.cancelled")?;
        self.builder
            .build_conditional_branch(stopping, cancelled, inspect)
            .llvm_ctx("select channel producer cancellation")?;
        self.builder.position_at_end(inspect);
        let send = coro::external(
            self.llvm,
            "hew_channel_send_native",
            self.ctx.i32_type().fn_type(
                &[
                    pointer.into(),
                    pointer.into(),
                    pointer.into(),
                    pointer.into(),
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
                witness.into(),
            ],
            "channel.send.status",
        )?
        .into_int_value();
        // A closed channel is not a fault: the element stays the producer's,
        // and the send is a no-op the ordinary scope exit cleans up.
        self.builder
            .build_switch(status, sent, &[(self.ctx.i32_type().const_zero(), wait)])
            .llvm_ctx("dispatch channel send outcome")?;
        self.builder.position_at_end(wait);
        frame.suspend(self.ctx, self.llvm, &self.builder, poll, invalid, false)?;
        self.builder.position_at_end(invalid);
        self.reject_invalid_task_state()?;
        self.builder.position_at_end(sent);
        self.emit_edge(normal)?;
        self.builder.position_at_end(cancelled);
        self.initialize_cancellation_fault()?;
        self.emit_edge(cancel)?;
        // A channel send never traps: a closed channel resumes normally. The
        // unwind edge exists for the frame's own cleanup dispatch.
        let _ = unwind;
        Ok(())
    }
}
