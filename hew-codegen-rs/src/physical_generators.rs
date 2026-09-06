//! Native generator suspension over the shared LLVM frame and callable ABI.

use super::{
    coro, llvm_type, suspend, AddressSpace, ArgumentTransfer, BasicValueEnum, CallableId,
    CodegenError, CodegenResult, FunctionEmitter, IntPredicate, IntValue, LlvmResultExt,
    ModuleEmitter, PhysicalEdge, PhysicalOp, StorageId,
};

fn descriptor(callable: CallableId, storage: StorageId, part: &str) -> String {
    format!("__hew_generator_{}_{}_{part}", callable.0, storage.0)
}

impl ModuleEmitter<'_, '_> {
    pub(super) fn emit_generator_descriptors(&self) -> CodegenResult<()> {
        for function in &self.module.functions {
            for operation in function.blocks.iter().flat_map(|block| &block.ops) {
                if let PhysicalOp::GeneratorMake {
                    dest,
                    yielded,
                    returned,
                    ..
                } = operation
                {
                    self.emit_value_descriptor(
                        &descriptor(function.callable, *dest, "yield"),
                        yielded,
                    )?;
                    self.emit_value_descriptor(
                        &descriptor(function.callable, *dest, "return"),
                        returned,
                    )?;
                }
            }
        }
        Ok(())
    }
}

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    fn generator_frame(&self) -> CodegenResult<&coro::Frame<'ctx>> {
        self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed(
                "generator operation requires its checked resumable frame".into(),
            )
        })
    }

    fn reject_generator_destroy(&self) -> CodegenResult<()> {
        let abort = coro::external(self.llvm, "abort", self.ctx.void_type().fn_type(&[], false))?;
        self.builder
            .build_call(abort, &[], "")
            .llvm_ctx("reject destruction before generator drain")?;
        self.builder
            .build_unreachable()
            .llvm_ctx("reject premature generator frame destruction")?;
        Ok(())
    }

    pub(super) fn emit_generator_make(
        &self,
        callable: StorageId,
        dest: StorageId,
    ) -> CodegenResult<()> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let yielded = self
            .llvm
            .get_global(&descriptor(self.function.callable, dest, "yield"))
            .ok_or_else(|| {
                CodegenError::FailClosed("generator yield descriptor is absent".into())
            })?;
        let returned = self
            .llvm
            .get_global(&descriptor(self.function.callable, dest, "return"))
            .ok_or_else(|| {
                CodegenError::FailClosed("generator return descriptor is absent".into())
            })?;
        let make = coro::external(
            self.llvm,
            "hew_checked_generator_new",
            pointer.fn_type(&[pointer.into(); 3], false),
        )?;
        let generator = suspend::call_value(
            &self.builder,
            make,
            &[
                self.slots[callable.0 as usize].into(),
                yielded.as_pointer_value().into(),
                returned.as_pointer_value().into(),
            ],
            "generator",
        )?;
        self.clear_owned(callable)?;
        self.store(dest, generator)
    }

    pub(super) fn emit_generator_yield(
        &self,
        value: &ArgumentTransfer,
        normal: &PhysicalEdge,
        cancel: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let ArgumentTransfer::Move(value) = value else {
            return Err(CodegenError::FailClosed(
                "yield must move its checked value".into(),
            ));
        };
        let frame = self.generator_frame()?;
        let output = self.result_out.ok_or_else(|| {
            CodegenError::FailClosed("generator body lacks its shared output slot".into())
        })?;
        let yielded = self.load(*value, "generator.yield.value")?;
        self.builder
            .build_store(output, yielded)
            .llvm_ctx("transfer yielded value to its consumer")?;
        self.clear_owned(*value)?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let publish = coro::external(
            self.llvm,
            "hew_coro_state_publish",
            self.ctx
                .i32_type()
                .fn_type(&[pointer.into(), self.ctx.i32_type().into()], false),
        )?;
        self.builder
            .build_call(
                publish,
                &[
                    frame.state.into(),
                    self.ctx.i32_type().const_int(4, false).into(),
                ],
                "",
            )
            .llvm_ctx("publish initialized generator yield")?;
        let resume = self
            .ctx
            .append_basic_block(self.value, "generator.yield.resume");
        let cancelled = self
            .ctx
            .append_basic_block(self.value, "generator.yield.cancelled");
        let invalid = self
            .ctx
            .append_basic_block(self.value, "generator.yield.invalid.destroy");
        frame.suspend(self.ctx, self.llvm, &self.builder, resume, invalid, false)?;
        self.builder.position_at_end(invalid);
        self.reject_generator_destroy()?;
        self.builder.position_at_end(resume);
        let status = self.state_value("hew_coro_state_is_cancelled", frame.state)?;
        let requested = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                status,
                self.ctx.i32_type().const_zero(),
                "generator.cancel.requested",
            )
            .llvm_ctx("observe generator cancellation")?;
        let continuing = self
            .ctx
            .append_basic_block(self.value, "generator.yield.continue");
        self.builder
            .build_conditional_branch(requested, cancelled, continuing)
            .llvm_ctx("resume producer or run its cancellation cleanup")?;
        self.builder.position_at_end(cancelled);
        self.initialize_cancellation_fault()?;
        self.emit_edge(cancel)?;
        self.builder.position_at_end(continuing);
        self.emit_edge(normal)
    }

    fn poll_generator(
        &self,
        generator: BasicValueEnum<'ctx>,
        closing: IntValue<'ctx>,
    ) -> CodegenResult<IntValue<'ctx>> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let poll = coro::external(
            self.llvm,
            "hew_checked_generator_poll",
            self.ctx.i32_type().fn_type(
                &[pointer.into(), pointer.into(), self.ctx.bool_type().into()],
                false,
            ),
        )?;
        Ok(suspend::call_value(
            &self.builder,
            poll,
            &[
                generator.into(),
                self.generator_frame()?.state.into(),
                closing.into(),
            ],
            "generator.status",
        )?
        .into_int_value())
    }

    fn take_generator_fault(&self, generator: BasicValueEnum<'ctx>) -> CodegenResult<()> {
        self.take_cleanup_fault(generator, "hew_checked_generator_take_fault")
    }

    fn take_cleanup_fault(
        &self,
        generator: BasicValueEnum<'ctx>,
        symbol: &str,
    ) -> CodegenResult<()> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let child_slot = self
            .value_emitter()
            .entry_scratch(pointer.into(), "generator.fault.slot")?;
        self.builder
            .build_store(child_slot, pointer.const_null())
            .llvm_ctx("initialize producer fault slot")?;
        let take = coro::external(
            self.llvm,
            symbol,
            self.ctx.i32_type().fn_type(&[pointer.into(); 2], false),
        )?;
        let child_status = suspend::call_value(
            &self.builder,
            take,
            &[generator.into(), child_slot.into()],
            "generator.fault.status",
        )?;
        let primary = self
            .builder
            .build_load(pointer, self.active_fault, "generator.primary.fault")
            .llvm_ctx("load current cleanup fault")?
            .into_pointer_value();
        let primary_status = self
            .builder
            .build_load(
                self.ctx.i32_type(),
                self.active_status,
                "generator.primary.status",
            )
            .llvm_ctx("load current cleanup status")?;
        let child = self
            .builder
            .build_load(pointer, child_slot, "generator.child.fault")
            .llvm_ctx("load producer fault")?;
        let present = self
            .builder
            .build_is_not_null(primary, "generator.primary.present")
            .llvm_ctx("test current cleanup fault")?;
        let status = self
            .builder
            .build_select(
                present,
                primary_status,
                child_status,
                "generator.combined.status",
            )
            .llvm_ctx("retain first cleanup failure")?;
        let combine = coro::external(
            self.llvm,
            "hew_fault_combine",
            pointer.fn_type(&[pointer.into(); 2], false),
        )?;
        let combined = suspend::call_value(
            &self.builder,
            combine,
            &[primary.into(), child.into()],
            "generator.combined.fault",
        )?;
        self.builder
            .build_store(self.active_fault, combined)
            .llvm_ctx("retain combined producer fault")?;
        self.builder
            .build_store(self.active_status, status)
            .llvm_ctx("retain combined producer status")?;
        Ok(())
    }

    pub(super) fn emit_generator_next(
        &self,
        generator: &ArgumentTransfer,
        result: StorageId,
        normal: &PhysicalEdge,
        cancel: &PhysicalEdge,
        unwind: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let ArgumentTransfer::BorrowMut(generator) = generator else {
            return Err(CodegenError::FailClosed(
                "generator iteration requires an exclusive receiver".into(),
            ));
        };
        let handle = self.load(*generator, "generator.receiver")?;
        let frame = self.generator_frame()?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let option = self
            .module
            .variant_glue
            .iter()
            .find(|glue| glue.ty == self.storage(result).unwrap().ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("generator next lacks its Option recipe".into())
            })?;
        let poll = self
            .ctx
            .append_basic_block(self.value, "generator.next.poll");
        let wait = self
            .ctx
            .append_basic_block(self.value, "generator.next.wait");
        let invalid = self
            .ctx
            .append_basic_block(self.value, "generator.next.invalid.destroy");
        let yielded = self
            .ctx
            .append_basic_block(self.value, "generator.next.yielded");
        let take = self
            .ctx
            .append_basic_block(self.value, "generator.next.take");
        let done = self
            .ctx
            .append_basic_block(self.value, "generator.next.done");
        let none = self
            .ctx
            .append_basic_block(self.value, "generator.next.none");
        let failed = self
            .ctx
            .append_basic_block(self.value, "generator.next.failed");
        let cancelled = self
            .ctx
            .append_basic_block(self.value, "generator.next.cancelled");
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("begin generator iteration")?;
        self.builder.position_at_end(poll);
        let cancelled_status = self.state_value("hew_coro_state_is_cancelled", frame.state)?;
        let closing = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                cancelled_status,
                self.ctx.i32_type().const_zero(),
                "generator.consumer.cancelled",
            )
            .llvm_ctx("observe consumer cancellation")?;
        let status = self.poll_generator(handle, closing)?;
        self.builder
            .build_switch(
                status,
                failed,
                &[
                    (self.ctx.i32_type().const_int(0, false), wait),
                    (self.ctx.i32_type().const_int(1, false), done),
                    (self.ctx.i32_type().const_int(3, false), cancelled),
                    (self.ctx.i32_type().const_int(4, false), yielded),
                ],
            )
            .llvm_ctx("dispatch checked generator outcome")?;
        self.builder.position_at_end(wait);
        frame.suspend(self.ctx, self.llvm, &self.builder, poll, invalid, false)?;
        self.builder.position_at_end(invalid);
        self.reject_generator_destroy()?;
        self.builder.position_at_end(yielded);
        self.builder
            .build_conditional_branch(closing, poll, take)
            .llvm_ctx("drain an unwanted yield after cancellation")?;
        self.builder.position_at_end(take);
        let yield_layout = self
            .module
            .target
            .layout(&option.variants[0].fields[0].ty)
            .ok_or_else(|| CodegenError::FailClosed("yield type has no physical layout".into()))?;
        let value_ty = llvm_type(self.ctx, &yield_layout.repr)?;
        let value_slot = self
            .value_emitter()
            .entry_scratch(value_ty, "generator.yield.slot")?;
        let take_fn = coro::external(
            self.llvm,
            "hew_checked_generator_take",
            self.ctx.void_type().fn_type(&[pointer.into(); 2], false),
        )?;
        self.builder
            .build_call(take_fn, &[handle.into(), value_slot.into()], "")
            .llvm_ctx("take the unique yielded value")?;
        let value = self
            .builder
            .build_load(value_ty, value_slot, "generator.yield")
            .llvm_ctx("load transferred yield")?;
        self.write_variant_value(self.slots[result.0 as usize], 0, &[value], option.id)?;
        self.set_place_initialized(result, true)?;
        self.emit_edge(normal)?;
        self.builder.position_at_end(done);
        self.builder
            .build_conditional_branch(closing, cancelled, none)
            .llvm_ctx("classify completed consumer cancellation")?;
        self.builder.position_at_end(none);
        self.write_variant_value(self.slots[result.0 as usize], 1, &[], option.id)?;
        self.set_place_initialized(result, true)?;
        self.emit_edge(normal)?;
        self.builder.position_at_end(cancelled);
        self.take_generator_fault(handle)?;
        let missing = self
            .ctx
            .append_basic_block(self.value, "generator.cancel.missing");
        let propagate = self
            .ctx
            .append_basic_block(self.value, "generator.cancel.propagate");
        let fault = self
            .builder
            .build_load(pointer, self.active_fault, "generator.cancel.fault")
            .llvm_ctx("load producer cancellation fault")?
            .into_pointer_value();
        let absent = self
            .builder
            .build_is_null(fault, "generator.cancel.absent")
            .llvm_ctx("test absent producer cancellation")?;
        self.builder
            .build_conditional_branch(absent, missing, propagate)
            .llvm_ctx("retain producer fault or construct consumer cancellation")?;
        self.builder.position_at_end(missing);
        self.initialize_cancellation_fault()?;
        self.builder
            .build_unconditional_branch(propagate)
            .llvm_ctx("propagate consumer cancellation")?;
        self.builder.position_at_end(propagate);
        self.emit_edge(cancel)?;
        self.builder.position_at_end(failed);
        let cancelled_failure = self
            .ctx
            .append_basic_block(self.value, "generator.failure.cancelled");
        let ordinary_failure = self
            .ctx
            .append_basic_block(self.value, "generator.failure.ordinary");
        self.builder
            .build_conditional_branch(closing, cancelled_failure, ordinary_failure)
            .llvm_ctx("preserve consumer cancellation as the primary failure")?;
        self.builder.position_at_end(cancelled_failure);
        self.initialize_cancellation_fault()?;
        self.take_generator_fault(handle)?;
        self.emit_edge(cancel)?;
        self.builder.position_at_end(ordinary_failure);
        self.take_generator_fault(handle)?;
        self.emit_edge(unwind)
    }

    pub(super) fn emit_value_close(
        &self,
        owner: StorageId,
        destroy: Option<super::DestroyAction>,
        conditional: bool,
        next: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let frame = self.generator_frame()?;
        let context = self
            .value_emitter()
            .entry_scratch(pointer.into(), "close.collector")?;
        self.builder
            .build_store(context, pointer.const_null())
            .llvm_ctx("initialize child collector")?;
        let leaves = if conditional {
            self.function.place_storage.get(&owner).map(|projection| {
                projection
                    .leaves
                    .iter()
                    .rev()
                    .map(|leaf| (leaf.storage, leaf.destroy))
                    .collect::<Vec<_>>()
            })
        } else {
            None
        };
        for (storage, action) in leaves.unwrap_or_else(|| vec![(owner, destroy)]) {
            let Some(action) = action else {
                continue;
            };
            let visit = self.ctx.append_basic_block(self.value, "close.visit");
            let after = self.ctx.append_basic_block(self.value, "close.after");
            if conditional {
                self.builder
                    .build_conditional_branch(self.place_initialized(storage)?, visit, after)
                    .llvm_ctx("select initialized owner")?;
            } else {
                self.builder
                    .build_unconditional_branch(visit)
                    .llvm_ctx("select owned value")?;
            }
            self.builder.position_at_end(visit);
            self.value_emitter().visit_close(
                self.slots[storage.0 as usize],
                &self.storage(storage)?.layout,
                action,
                context,
            )?;
            self.builder
                .build_unconditional_branch(after)
                .llvm_ctx("finish child selection")?;
            self.builder.position_at_end(after);
        }
        let collector = self
            .builder
            .build_load(pointer, context, "close.children")
            .llvm_ctx("load selected children")?;
        let poll = self.ctx.append_basic_block(self.value, "close.poll");
        let wait = self.ctx.append_basic_block(self.value, "close.wait");
        let done = self.ctx.append_basic_block(self.value, "close.done");
        let invalid = self
            .ctx
            .append_basic_block(self.value, "close.invalid.destroy");
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("begin cooperative child cleanup")?;
        self.builder.position_at_end(poll);
        let poll_fn = coro::external(
            self.llvm,
            "hew_value_close_poll",
            self.ctx.i32_type().fn_type(&[pointer.into(); 2], false),
        )?;
        let status = suspend::call_value(
            &self.builder,
            poll_fn,
            &[collector.into(), frame.state.into()],
            "close.status",
        )?
        .into_int_value();
        self.builder
            .build_switch(status, done, &[(self.ctx.i32_type().const_zero(), wait)])
            .llvm_ctx("wait for child cleanup")?;
        self.builder.position_at_end(wait);
        frame.suspend(self.ctx, self.llvm, &self.builder, poll, invalid, false)?;
        self.builder.position_at_end(invalid);
        self.reject_generator_destroy()?;
        self.builder.position_at_end(done);
        self.take_cleanup_fault(collector, "hew_value_close_finish")?;
        self.emit_edge(next)
    }
}
