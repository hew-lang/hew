//! Compiler-owned environment masks and capture copy/drop callbacks.

use super::*;
use hew_mir::physical::PhysicalEnvironmentGlue;

pub(super) fn environment_descriptor_symbol(index: usize) -> String {
    format!("__hew_callable_environment_{index}")
}

impl<'ctx> ModuleEmitter<'ctx, '_> {
    pub(super) fn emit_environment_descriptors(&self) -> CodegenResult<()> {
        for (index, glue) in self.module.environment_glue.iter().enumerate() {
            self.emit_environment_descriptor(&environment_descriptor_symbol(index), glue)?;
        }
        Ok(())
    }

    fn emit_environment_descriptor(
        &self,
        name: &str,
        glue: &PhysicalEnvironmentGlue,
    ) -> CodegenResult<()> {
        let target = TargetData::create(&self.module.target.data_layout);
        let layout = self
            .module
            .target
            .environment_layout(&glue.ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("callable environment lacks its target layout".into())
            })?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let size = self.ctx.ptr_sized_int_type(&target, None);
        let (clone, drop, ownership) = if glue.fields.is_empty() {
            (
                pointer.const_null(),
                pointer.const_null(),
                HewTypeOwnershipKind::Plain,
            )
        } else {
            let drop = self.emit_environment_drop(&format!("{name}_drop"), glue, layout)?;
            let clone = if glue.cloneable {
                self.emit_environment_clone(&format!("{name}_clone"), glue, layout, drop)?
                    .as_global_value()
                    .as_pointer_value()
            } else {
                pointer.const_null()
            };
            (
                clone,
                drop.as_global_value().as_pointer_value(),
                HewTypeOwnershipKind::LayoutManaged,
            )
        };
        let descriptor_ty = value_descriptor_type(self.ctx, &target);
        let descriptor = descriptor_ty.const_named_struct(&[
            size.const_int(layout.size, false).into(),
            size.const_int(u64::from(layout.align), false).into(),
            self.ctx.i8_type().const_int(ownership as u64, false).into(),
            clone.into(),
            drop.into(),
            self.emit_environment_close(&format!("{name}_close"), glue, layout)?
                .into(),
        ]);
        let global = self.llvm.add_global(descriptor_ty, None, name);
        global.set_linkage(Linkage::Internal);
        global.set_constant(true);
        global.set_initializer(&descriptor);
        Ok(())
    }

    fn emit_environment_close(
        &self,
        name: &str,
        glue: &PhysicalEnvironmentGlue,
        layout: &PhysicalLayout,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        if glue.fields.is_empty() {
            return Ok(pointer.const_null());
        }
        let function = self.llvm.add_function(
            name,
            self.ctx.void_type().fn_type(&[pointer.into(); 2], false),
            Some(Linkage::Internal),
        );
        let environment = function.get_nth_param(0).unwrap().into_pointer_value();
        let context = function.get_nth_param(1).unwrap().into_pointer_value();
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(function, "entry"));
        let emitter = ValueEmitter {
            module: self.module,
            ctx: self.ctx,
            llvm: &self.llvm,
            builder: &builder,
            value: function,
        };
        for (index, field) in glue.fields.iter().enumerate().rev() {
            let Some(action) = field.destroy else {
                continue;
            };
            let index = u32::try_from(index)
                .map_err(|_| CodegenError::FailClosed("capture index exceeds u32".into()))?;
            let live = environment_mask_bit(self.ctx, &builder, environment, index)?;
            let visit = self.ctx.append_basic_block(function, "capture.close");
            let next = self.ctx.append_basic_block(function, "capture.next");
            builder
                .build_conditional_branch(live, visit, next)
                .llvm_ctx("select initialized capture")?;
            builder.position_at_end(visit);
            let slot = environment_field(self.ctx, &builder, layout, environment, index)?;
            let field_layout =
                self.module.target.layout(&field.ty).ok_or_else(|| {
                    CodegenError::FailClosed("capture cleanup lacks layout".into())
                })?;
            emitter.visit_close(slot, field_layout, action, context)?;
            builder
                .build_unconditional_branch(next)
                .llvm_ctx("finish capture selection")?;
            builder.position_at_end(next);
        }
        builder
            .build_return(None)
            .llvm_ctx("finish environment child selection")?;
        Ok(function.as_global_value().as_pointer_value())
    }

    fn emit_environment_drop(
        &self,
        name: &str,
        glue: &PhysicalEnvironmentGlue,
        layout: &PhysicalLayout,
    ) -> CodegenResult<FunctionValue<'ctx>> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let function = self.llvm.add_function(
            name,
            self.ctx.void_type().fn_type(&[pointer.into()], false),
            Some(Linkage::Internal),
        );
        let environment = function.get_first_param().unwrap().into_pointer_value();
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(function, "entry"));
        let emitter = ValueEmitter {
            module: self.module,
            ctx: self.ctx,
            llvm: &self.llvm,
            builder: &builder,
            value: function,
        };
        for (index, field) in glue.fields.iter().enumerate().rev() {
            let index = u32::try_from(index)
                .map_err(|_| CodegenError::FailClosed("capture field index exceeds u32".into()))?;
            let live = environment_mask_bit(self.ctx, &builder, environment, index)?;
            let destroy = self.ctx.append_basic_block(function, "capture.drop");
            let next = self.ctx.append_basic_block(function, "capture.next");
            builder
                .build_conditional_branch(live, destroy, next)
                .llvm_ctx("test live capture")?;
            builder.position_at_end(destroy);
            // Clear first so recursive destruction never sees this owner again.
            set_environment_mask_bit(self.ctx, &builder, environment, index, false)?;
            if let Some(action) = field.destroy {
                let slot = environment_field(self.ctx, &builder, layout, environment, index)?;
                let field_layout = self.module.target.layout(&field.ty).ok_or_else(|| {
                    CodegenError::FailClosed("capture drop lacks its value layout".into())
                })?;
                let value = builder
                    .build_load(
                        llvm_type(self.ctx, &field_layout.repr)?,
                        slot,
                        "capture.owner",
                    )
                    .llvm_ctx("load initialized capture")?;
                emitter.destroy_loaded_value(value, field_layout, action)?;
            }
            builder
                .build_unconditional_branch(next)
                .llvm_ctx("finish capture destruction")?;
            builder.position_at_end(next);
        }
        builder
            .build_return(None)
            .llvm_ctx("finish environment destruction")?;
        Ok(function)
    }

    fn emit_environment_clone(
        &self,
        name: &str,
        glue: &PhysicalEnvironmentGlue,
        layout: &PhysicalLayout,
        drop: FunctionValue<'ctx>,
    ) -> CodegenResult<FunctionValue<'ctx>> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let function = self.llvm.add_function(
            name,
            self.ctx
                .i32_type()
                .fn_type(&[pointer.into(), pointer.into()], false),
            Some(Linkage::Internal),
        );
        let source = function.get_nth_param(0).unwrap().into_pointer_value();
        let destination = function.get_nth_param(1).unwrap().into_pointer_value();
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(function, "entry"));
        // The value protocol copied all bytes, including source ownership bits.
        // Destination ownership starts empty until each field copy succeeds.
        for byte in 0..glue.fields.len().div_ceil(8) {
            let slot = mask_byte(
                self.ctx,
                &builder,
                destination,
                u32::try_from(byte).map_err(|_| {
                    CodegenError::FailClosed("capture mask exceeds u32 bytes".into())
                })?,
            )?;
            builder
                .build_store(slot, self.ctx.i8_type().const_zero())
                .llvm_ctx("reset copied capture mask")?;
        }
        for (index, field) in glue.fields.iter().enumerate() {
            let index = u32::try_from(index)
                .map_err(|_| CodegenError::FailClosed("capture field index exceeds u32".into()))?;
            let live = environment_mask_bit(self.ctx, &builder, source, index)?;
            let copy = self.ctx.append_basic_block(function, "capture.clone");
            let next = self.ctx.append_basic_block(function, "capture.next");
            builder
                .build_conditional_branch(live, copy, next)
                .llvm_ctx("test source capture")?;
            builder.position_at_end(copy);
            let action = field.clone.ok_or_else(|| {
                CodegenError::FailClosed(
                    "cloneable environment contains a noncloneable capture".into(),
                )
            })?;
            if action != CloneAction::Bitwise {
                let field_layout = self.module.target.layout(&field.ty).ok_or_else(|| {
                    CodegenError::FailClosed("capture clone lacks its value layout".into())
                })?;
                let callback = self.emit_value_clone_callback(
                    &format!("{name}_field_{index}"),
                    field_layout,
                    action,
                )?;
                let src = environment_field(self.ctx, &builder, layout, source, index)?;
                let dst = environment_field(self.ctx, &builder, layout, destination, index)?;
                let status = builder
                    .build_indirect_call(
                        self.ctx
                            .i32_type()
                            .fn_type(&[pointer.into(), pointer.into()], false),
                        callback,
                        &[src.into(), dst.into()],
                        "capture.clone.status",
                    )
                    .llvm_ctx("clone environment capture")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| {
                        CodegenError::FailClosed("capture clone returned no status".into())
                    })?
                    .into_int_value();
                let ok = builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        status,
                        self.ctx.i32_type().const_zero(),
                        "capture.clone.ok",
                    )
                    .llvm_ctx("check capture clone status")?;
                let complete = self.ctx.append_basic_block(function, "capture.complete");
                let failed = self.ctx.append_basic_block(function, "capture.failed");
                builder
                    .build_conditional_branch(ok, complete, failed)
                    .llvm_ctx("branch on capture copy")?;
                builder.position_at_end(failed);
                builder
                    .build_call(drop, &[destination.into()], "")
                    .llvm_ctx("roll back initialized capture copies")?;
                builder
                    .build_return(Some(&status))
                    .llvm_ctx("forward capture clone failure")?;
                builder.position_at_end(complete);
            }
            set_environment_mask_bit(self.ctx, &builder, destination, index, true)?;
            builder
                .build_unconditional_branch(next)
                .llvm_ctx("finish capture copy")?;
            builder.position_at_end(next);
        }
        builder
            .build_return(Some(&self.ctx.i32_type().const_zero()))
            .llvm_ctx("finish environment clone")?;
        Ok(function)
    }
}

fn environment_field<'ctx>(
    ctx: &'ctx Context,
    builder: &Builder<'ctx>,
    layout: &PhysicalLayout,
    environment: PointerValue<'ctx>,
    index: u32,
) -> CodegenResult<PointerValue<'ctx>> {
    builder
        .build_struct_gep(
            llvm_type(ctx, &layout.repr)?.into_struct_type(),
            environment,
            index + 1,
            "capture.slot",
        )
        .llvm_ctx("address aligned environment capture")
}

fn mask_byte<'ctx>(
    ctx: &'ctx Context,
    builder: &Builder<'ctx>,
    environment: PointerValue<'ctx>,
    byte: u32,
) -> CodegenResult<PointerValue<'ctx>> {
    // SAFETY: the mask is the first field and the caller bounds this byte by
    // the exact capture count used to construct its environment layout.
    unsafe {
        builder.build_in_bounds_gep(
            ctx.i8_type(),
            environment,
            &[ctx.i32_type().const_int(u64::from(byte), false)],
            "capture.mask.byte",
        )
    }
    .llvm_ctx("address environment initialization mask")
}

pub(super) fn environment_mask_bit<'ctx>(
    ctx: &'ctx Context,
    builder: &Builder<'ctx>,
    environment: PointerValue<'ctx>,
    field: u32,
) -> CodegenResult<IntValue<'ctx>> {
    let slot = mask_byte(ctx, builder, environment, field / 8)?;
    let byte = builder
        .build_load(ctx.i8_type(), slot, "capture.mask")
        .llvm_ctx("load capture mask")?
        .into_int_value();
    let bit = builder
        .build_and(
            byte,
            ctx.i8_type().const_int(1 << (field % 8), false),
            "capture.bit",
        )
        .llvm_ctx("extract capture initialization bit")?;
    builder
        .build_int_compare(
            IntPredicate::NE,
            bit,
            ctx.i8_type().const_zero(),
            "capture.live",
        )
        .llvm_ctx("test capture initialization")
}

pub(super) fn set_environment_mask_bit<'ctx>(
    ctx: &'ctx Context,
    builder: &Builder<'ctx>,
    environment: PointerValue<'ctx>,
    field: u32,
    initialized: bool,
) -> CodegenResult<()> {
    let slot = mask_byte(ctx, builder, environment, field / 8)?;
    let byte = builder
        .build_load(ctx.i8_type(), slot, "capture.mask")
        .llvm_ctx("load capture mask")?
        .into_int_value();
    let bit = 1_u8 << (field % 8);
    let updated = if initialized {
        builder.build_or(
            byte,
            ctx.i8_type().const_int(u64::from(bit), false),
            "capture.init",
        )
    } else {
        builder.build_and(
            byte,
            ctx.i8_type().const_int(u64::from(!bit), false),
            "capture.take",
        )
    }
    .llvm_ctx("update capture ownership bit")?;
    builder
        .build_store(slot, updated)
        .llvm_ctx("publish capture ownership bit")?;
    Ok(())
}

pub(super) fn callable_clone_function<'ctx>(
    ctx: &'ctx Context,
    llvm: &Module<'ctx>,
) -> CodegenResult<FunctionValue<'ctx>> {
    let pointer = ctx.ptr_type(AddressSpace::default());
    get_or_declare_external(
        llvm,
        "hew_callable_clone",
        ctx.i32_type()
            .fn_type(&[pointer.into(), pointer.into()], false),
    )
}

impl<'ctx> ValueEmitter<'_, 'ctx> {
    pub(super) fn clone_callable_value(
        &self,
        value: BasicValueEnum<'ctx>,
        layout: &PhysicalLayout,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let ty = llvm_type(self.ctx, &layout.repr)?;
        let source = self.entry_scratch(ty, "callable.clone.source")?;
        let destination = self.entry_scratch(ty, "callable.clone.destination")?;
        self.builder
            .build_store(source, value)
            .llvm_ctx("stage callable clone source")?;
        let clone = callable_clone_function(self.ctx, self.llvm)?;
        let status = self
            .builder
            .build_call(
                clone,
                &[source.into(), destination.into()],
                "callable.clone.status",
            )
            .llvm_ctx("clone independent callable environment")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("callable clone returned no status".into()))?
            .into_int_value();
        let ok = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_zero(),
                "callable.clone.ok",
            )
            .llvm_ctx("check callable clone result")?;
        let success = self
            .ctx
            .append_basic_block(self.value, "callable.clone.success");
        let failure = self
            .ctx
            .append_basic_block(self.value, "callable.clone.failure");
        self.builder
            .build_conditional_branch(ok, success, failure)
            .llvm_ctx("branch on callable clone status")?;
        self.builder.position_at_end(failure);
        let abort =
            get_or_declare_external(self.llvm, "abort", self.ctx.void_type().fn_type(&[], false))?;
        self.builder
            .build_call(abort, &[], "")
            .llvm_ctx("abort failed semantic callable copy")?;
        self.builder
            .build_unreachable()
            .llvm_ctx("terminate failed callable copy")?;
        self.builder.position_at_end(success);
        self.builder
            .build_load(ty, destination, "callable.clone.value")
            .llvm_ctx("load initialized callable copy")
    }
}

use hew_mir::physical::{ClosureId, PhysicalCallSignature, PhysicalClosure, StorageOrigin};

fn callable_descriptor_type(ctx: &Context) -> inkwell::types::StructType<'_> {
    let pointer = ctx.ptr_type(AddressSpace::default());
    ctx.struct_type(&[pointer.into(), pointer.into(), pointer.into()], false)
}

fn callable_carrier<'ctx>(
    ctx: &'ctx Context,
    builder: &Builder<'ctx>,
    environment: PointerValue<'ctx>,
    descriptor: PointerValue<'ctx>,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    let pointer = ctx.ptr_type(AddressSpace::default());
    let ty = ctx.struct_type(&[pointer.into(), pointer.into()], false);
    let value = builder
        .build_insert_value(ty.get_undef(), environment, 0, "callable.environment")
        .llvm_ctx("initialize callable environment pointer")?
        .into_struct_value();
    Ok(builder
        .build_insert_value(value, descriptor, 1, "callable.descriptor")
        .llvm_ctx("initialize callable descriptor pointer")?
        .into_struct_value()
        .into())
}

fn function_descriptor_symbol(id: CallableId) -> String {
    format!("__hew_callable_function_{}", id.0)
}
fn closure_descriptor_symbol(id: ClosureId) -> String {
    format!("__hew_callable_closure_{}", id.0)
}

impl<'ctx> ModuleEmitter<'ctx, '_> {
    pub(super) fn emit_callable_descriptors(&self) -> CodegenResult<()> {
        for closure in &self.module.closures {
            let body = callable(self.module, closure.body)?;
            self.emit_callable_descriptor(
                &closure_descriptor_symbol(closure.id),
                body,
                &closure.ty,
                Some(closure),
            )?;
        }
        let mut functions = BTreeMap::new();
        for function in &self.module.functions {
            for op in function.blocks.iter().flat_map(|block| &block.ops) {
                if let PhysicalOp::FunctionMake { dest, callee } = op {
                    functions.insert(*callee, &function.storage[dest.0 as usize].ty);
                }
            }
        }
        for (id, ty) in functions {
            self.emit_callable_descriptor(
                &function_descriptor_symbol(id),
                callable(self.module, id)?,
                ty,
                None,
            )?;
        }
        Ok(())
    }

    fn emit_callable_descriptor(
        &self,
        name: &str,
        body: &PhysicalCallable,
        ty: &ResolvedTy,
        closure: Option<&PhysicalClosure>,
    ) -> CodegenResult<()> {
        let index = self
            .module
            .environment_glue
            .iter()
            .position(|glue| &glue.ty == ty)
            .ok_or_else(|| {
                CodegenError::FailClosed(
                    "callable descriptor lacks its exact environment recipe".into(),
                )
            })?;
        let environment = self
            .llvm
            .get_global(&environment_descriptor_symbol(index))
            .ok_or_else(|| {
                CodegenError::FailClosed(
                    "callable descriptor lacks its environment layout global".into(),
                )
            })?;
        let descriptor_ty = callable_descriptor_type(self.ctx);
        let descriptor = self.llvm.add_global(descriptor_ty, None, name);
        descriptor.set_linkage(Linkage::Internal);
        descriptor.set_constant(true);
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let owned_body = closure.is_some()
            && body.params[0].passing == hew_mir::physical::SemParamPassing::Consume;
        let borrow = if owned_body {
            pointer.const_null()
        } else {
            self.emit_invoke_adapter(
                &format!("{name}_borrow"),
                descriptor.as_pointer_value(),
                body,
                closure.is_some(),
                false,
            )?
            .as_global_value()
            .as_pointer_value()
        };
        let once = self.emit_invoke_adapter(
            &format!("{name}_once"),
            descriptor.as_pointer_value(),
            body,
            closure.is_some(),
            true,
        )?;
        descriptor.set_initializer(&descriptor_ty.const_named_struct(&[
            environment.as_pointer_value().into(),
            borrow.into(),
            once.as_global_value().as_pointer_value().into(),
        ]));
        Ok(())
    }

    fn emit_invoke_adapter(
        &self,
        name: &str,
        descriptor: PointerValue<'ctx>,
        body: &PhysicalCallable,
        has_receiver: bool,
        consuming: bool,
    ) -> CodegenResult<FunctionValue<'ctx>> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let function = self.llvm.add_function(
            name,
            pointer.fn_type(&[pointer.into(); 5], false),
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(function, "entry"));
        let environment = function.get_nth_param(0).unwrap().into_pointer_value();
        let slots = function.get_nth_param(1).unwrap().into_pointer_value();
        let result_out = function.get_nth_param(2).unwrap().into_pointer_value();
        let fault_out = function.get_nth_param(3).unwrap().into_pointer_value();
        let state = function.get_nth_param(4).unwrap().into_pointer_value();
        // Consuming bodies copy their receiver and ordinary arguments into
        // entry storage before suspension, then own all cleanup. Their adapters
        // can transfer that frame directly instead of allocating a second one.
        let transfers_receiver = has_receiver
            && consuming
            && body.params[0].passing == hew_mir::physical::SemParamPassing::Consume
            && body.params.iter().all(|parameter| {
                parameter.passing != hew_mir::physical::SemParamPassing::BorrowMut
            });
        let frame = if transfers_receiver {
            None
        } else {
            Some(coro::begin(
                self.ctx, &self.llvm, &builder, function, state,
            )?)
        };
        let mut arguments = Vec::<BasicMetadataValueEnum<'ctx>>::new();
        let receiver = if has_receiver {
            let carrier = callable_carrier(self.ctx, &builder, environment, descriptor)?;
            let slot = builder
                .build_alloca(carrier.get_type(), "receiver.slot")
                .llvm_ctx("allocate adapter receiver")?;
            builder
                .build_store(slot, carrier)
                .llvm_ctx("initialize adapter receiver")?;
            arguments.push(slot.into());
            Some(slot)
        } else {
            None
        };
        for (index, parameter) in body
            .params
            .iter()
            .skip(usize::from(has_receiver))
            .enumerate()
        {
            // SAFETY: the typed physical call supplies exactly this argument array.
            let argument = unsafe {
                builder.build_in_bounds_gep(
                    pointer,
                    slots,
                    &[self.ctx.i64_type().const_int(index as u64, false)],
                    "argument.slot.address",
                )
            }
            .llvm_ctx("address erased argument slot")?;
            let slot = builder
                .build_load(pointer, argument, "argument.slot")
                .llvm_ctx("load erased argument address")?
                .into_pointer_value();
            arguments.push(match parameter.carrier {
                ParamCarrier::Indirect => slot.into(),
                ParamCarrier::Direct => builder
                    .build_load(
                        llvm_type(self.ctx, &parameter.layout.repr)?,
                        slot,
                        "argument.value",
                    )
                    .llvm_ctx("load typed adapter argument")?
                    .into(),
            });
        }
        if body.return_layout.is_some() {
            arguments.push(result_out.into());
        }
        arguments.push(fault_out.into());
        if transfers_receiver && body.is_resumable {
            arguments.push(state.into());
            let child_frame =
                suspend::call_value(&builder, self.ramps[&body.id], &arguments, "invoke.frame")?;
            builder
                .build_return(Some(&child_frame))
                .llvm_ctx("transfer the owning callable continuation")?;
            return Ok(function);
        }
        let status = if body.is_resumable {
            let new_child = coro::external(
                &self.llvm,
                "hew_coro_state_child",
                pointer.fn_type(&[pointer.into()], false),
            )?;
            let child = suspend::call_value(&builder, new_child, &[state.into()], "invoke.child")?
                .into_pointer_value();
            arguments.push(child.into());
            let child_frame =
                suspend::call_value(&builder, self.ramps[&body.id], &arguments, "invoke.frame")?
                    .into_pointer_value();
            suspend::await_child(
                self.ctx,
                &self.llvm,
                &builder,
                function,
                frame.as_ref().expect("borrowed adapter owns a frame"),
                child,
                child_frame,
            )?
        } else {
            suspend::call_value(
                &builder,
                self.functions[&body.id],
                &arguments,
                "invoke.status",
            )?
            .into_int_value()
        };
        // Concrete consuming bodies destroy their Owned receiver on both exits.
        // A weakened borrowed body leaves that disposal to this once adapter.
        if consuming
            && has_receiver
            && body.params[0].passing != hew_mir::physical::SemParamPassing::Consume
        {
            let drop = external_drop(self.ctx, &self.llvm, "hew_callable_drop")?;
            builder
                .build_call(drop, &[receiver.unwrap().into()], "")
                .llvm_ctx("dispose weakened once receiver on either outcome")?;
        }
        let finish = coro::external(
            &self.llvm,
            "hew_coro_state_finish",
            self.ctx
                .i32_type()
                .fn_type(&[pointer.into(), self.ctx.i32_type().into()], false),
        )?;
        builder
            .build_call(finish, &[state.into(), status.into()], "")
            .llvm_ctx("publish callable adapter outcome")?;
        if let Some(frame) = frame {
            builder
                .build_unconditional_branch(frame.finish)
                .llvm_ctx("finish callable adapter frame")?;
        } else {
            builder
                .build_return(Some(&pointer.const_null()))
                .llvm_ctx("finish synchronous owning callable")?;
        }
        Ok(function)
    }
}

pub(super) fn capture_parameter_slot<'ctx>(
    module: &ModuleEmitter<'ctx, '_>,
    function: &PhysicalFunction,
    callable: &PhysicalCallable,
    value: FunctionValue<'ctx>,
    builder: &Builder<'ctx>,
    storage: &PhysicalStorage,
) -> CodegenResult<PointerValue<'ctx>> {
    let StorageOrigin::Capture { environment, field } = storage.origin else {
        unreachable!()
    };
    let index = function
        .parameters
        .iter()
        .position(|parameter| *parameter == environment)
        .ok_or_else(|| {
            CodegenError::FailClosed("capture environment is not a receiver parameter".into())
        })?;
    let parameter = &callable.params[index];
    let incoming = value
        .get_nth_param(
            u32::try_from(index)
                .map_err(|_| CodegenError::FailClosed("receiver index exceeds u32".into()))?,
        )
        .ok_or_else(|| CodegenError::FailClosed("missing capture receiver".into()))?
        .into_pointer_value();
    let carrier = builder
        .build_load(
            llvm_type(module.ctx, &parameter.layout.repr)?,
            incoming,
            "capture.receiver",
        )
        .llvm_ctx("load capture receiver carrier")?
        .into_struct_value();
    let environment = builder
        .build_extract_value(carrier, 0, "capture.environment")
        .llvm_ctx("load environment allocation")?
        .into_pointer_value();
    let layout = module
        .module
        .target
        .environment_layout(&parameter.ty)
        .ok_or_else(|| {
            CodegenError::FailClosed("capture receiver lacks environment layout".into())
        })?;
    environment_field(module.ctx, builder, layout, environment, field)
}

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    pub(super) fn set_capture_initialized(
        &self,
        id: StorageId,
        initialized: bool,
    ) -> CodegenResult<()> {
        if let StorageOrigin::Capture { environment, field } = self.storage(id)?.origin {
            let carrier = self
                .load(environment, "capture.receiver")?
                .into_struct_value();
            let allocation = self
                .builder
                .build_extract_value(carrier, 0, "capture.environment")
                .llvm_ctx("load capture allocation")?
                .into_pointer_value();
            set_environment_mask_bit(self.ctx, &self.builder, allocation, field, initialized)?;
        }
        Ok(())
    }

    pub(super) fn emit_function_make(&self, dest: StorageId, id: CallableId) -> CodegenResult<()> {
        let descriptor = self
            .llvm
            .get_global(&function_descriptor_symbol(id))
            .ok_or_else(|| {
                CodegenError::FailClosed("function value lacks its descriptor".into())
            })?;
        let carrier = callable_carrier(
            self.ctx,
            &self.builder,
            self.ctx.ptr_type(AddressSpace::default()).const_null(),
            descriptor.as_pointer_value(),
        )?;
        self.store(dest, carrier)
    }

    pub(super) fn emit_closure_make(
        &self,
        dest: StorageId,
        id: ClosureId,
        fields: &[StorageId],
    ) -> CodegenResult<()> {
        let descriptor = self
            .llvm
            .get_global(&closure_descriptor_symbol(id))
            .ok_or_else(|| CodegenError::FailClosed("closure value lacks its descriptor".into()))?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let alloc = get_or_declare_external(
            self.llvm,
            "hew_callable_env_alloc",
            pointer.fn_type(&[pointer.into()], false),
        )?;
        let environment = self
            .builder
            .build_call(
                alloc,
                &[descriptor.as_pointer_value().into()],
                "closure.allocate",
            )
            .llvm_ctx("allocate zeroed closure environment")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("closure allocation returned void".into()))?
            .into_pointer_value();
        let layout = self
            .module
            .target
            .environment_layout(&self.storage(dest)?.ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("closure construction lacks its environment layout".into())
            })?;
        for (index, field) in fields.iter().enumerate() {
            let index = u32::try_from(index)
                .map_err(|_| CodegenError::FailClosed("capture index exceeds u32".into()))?;
            let source = self.load(*field, "capture.acquire")?;
            let destination =
                environment_field(self.ctx, &self.builder, layout, environment, index)?;
            self.builder
                .build_store(destination, source)
                .llvm_ctx("initialize owned capture")?;
            self.clear_owned(*field)?;
            set_environment_mask_bit(self.ctx, &self.builder, environment, index, true)?;
        }
        self.store(
            dest,
            callable_carrier(
                self.ctx,
                &self.builder,
                environment,
                descriptor.as_pointer_value(),
            )?,
        )
    }

    pub(super) fn emit_indirect_call(
        &self,
        callee: ArgumentTransfer,
        signature: &PhysicalCallSignature,
        args: &[ArgumentTransfer],
        result: Option<StorageId>,
        normal: &PhysicalEdge,
        unwind: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        let source = argument_source(&callee);
        let carrier = self.load(source, "callable.receiver")?.into_struct_value();
        let environment = self
            .builder
            .build_extract_value(carrier, 0, "callable.environment")
            .llvm_ctx("load callable environment")?
            .into_pointer_value();
        let descriptor = self
            .builder
            .build_extract_value(carrier, 1, "callable.descriptor")
            .llvm_ctx("load callable descriptor")?
            .into_pointer_value();
        let consuming = matches!(callee, ArgumentTransfer::Move(_));
        let entry_slot = self
            .builder
            .build_struct_gep(
                callable_descriptor_type(self.ctx),
                descriptor,
                if consuming { 2 } else { 1 },
                "callable.entry.slot",
            )
            .llvm_ctx("select verified callable invocation mode")?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let invoke = self
            .builder
            .build_load(pointer, entry_slot, "callable.entry")
            .llvm_ctx("load exact invocation adapter")?
            .into_pointer_value();
        let mut moved = Vec::new();
        let argument_slots = if args.is_empty() {
            pointer.const_null()
        } else {
            let count = u32::try_from(args.len())
                .map_err(|_| CodegenError::FailClosed("argument array exceeds u32".into()))?;
            let slots = self
                .value_emitter()
                .entry_scratch(pointer.array_type(count).into(), "callable.arguments")?;
            for (index, (argument, parameter)) in args.iter().zip(&signature.params).enumerate() {
                let source = argument_source(argument);
                let address = if let ArgumentTransfer::Clone { action, .. } = argument {
                    let value = self.clone_value(source, *action)?;
                    let slot = self.value_emitter().entry_scratch(
                        llvm_type(self.ctx, &parameter.layout.repr)?,
                        "callable.argument.copy",
                    )?;
                    self.builder
                        .build_store(slot, value)
                        .llvm_ctx("stage copied argument")?;
                    slot
                } else {
                    if matches!(argument, ArgumentTransfer::Move(_)) {
                        moved.push(source);
                    }
                    self.slots[source.0 as usize]
                };
                // SAFETY: this index is within the allocated argument pointer array.
                let slot = unsafe {
                    self.builder.build_in_bounds_gep(
                        pointer,
                        slots,
                        &[self.ctx.i64_type().const_int(index as u64, false)],
                        "callable.argument.slot",
                    )
                }
                .llvm_ctx("address invocation argument slot")?;
                self.builder
                    .build_store(slot, address)
                    .llvm_ctx("store invocation argument address")?;
            }
            slots
        };
        if consuming {
            self.clear_owned(source)?;
        }
        self.builder
            .build_store(self.active_fault, pointer.const_null())
            .llvm_ctx("clear invocation fault")?;
        let result_address =
            result.map_or_else(|| pointer.const_null(), |id| self.slots[id.0 as usize]);
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("indirect invocation requires a resumable caller".into())
        })?;
        let new_child = coro::external(
            self.llvm,
            "hew_coro_state_child",
            pointer.fn_type(&[pointer.into()], false),
        )?;
        let child = suspend::call_value(
            &self.builder,
            new_child,
            &[frame.state.into()],
            "invoke.child",
        )?
        .into_pointer_value();
        let child_frame = self
            .builder
            .build_indirect_call(
                pointer.fn_type(&[pointer.into(); 5], false),
                invoke,
                &[
                    environment.into(),
                    argument_slots.into(),
                    result_address.into(),
                    self.active_fault.into(),
                    child.into(),
                ],
                "invoke.frame",
            )
            .llvm_ctx("start callable adapter")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("invocation returned no frame".into()))?
            .into_pointer_value();
        for source in &moved {
            self.clear_owned(*source)?;
        }
        let status = suspend::await_child(
            self.ctx,
            self.llvm,
            &self.builder,
            self.value,
            frame,
            child,
            child_frame,
        )?;
        self.builder
            .build_store(self.active_status, status)
            .llvm_ctx("retain invocation status")?;
        self.emit_call_outcome(status, result, normal, unwind)
    }
}
