//! Compiler-owned environment masks and capture copy/drop callbacks.

use super::*;
use hew_mir::physical::PhysicalEnvironmentGlue;

pub(super) fn environment_descriptor_symbol(index: usize) -> String {
    format!("__hew_callable_environment_{index}")
}

/// Acquire an independent code root through the canonical value descriptor.
pub(super) fn acquire_value_code<'ctx>(
    ctx: &'ctx Context,
    target_data: &TargetData,
    llvm: &Module<'ctx>,
    builder: &Builder<'ctx>,
    layout: PointerValue<'ctx>,
) -> CodegenResult<PointerValue<'ctx>> {
    let pointer = ctx.ptr_type(AddressSpace::default());
    let slot = builder
        .build_struct_gep(
            value_descriptor_type(ctx, target_data),
            layout,
            6,
            "code.owner.slot",
        )
        .llvm_ctx("address defining-code owner")?;
    let slot = builder
        .build_load(pointer, slot, "code.owner")
        .llvm_ctx("read defining-code owner")?
        .into_pointer_value();
    let retain = get_or_declare_external(
        llvm,
        "hew_code_root_acquire",
        pointer.fn_type(&[pointer.into()], false),
    )?;
    builder
        .build_call(retain, &[slot.into()], "code.root")
        .llvm_ctx("retain defining code")?
        .try_as_basic_value()
        .basic()
        .map(BasicValueEnum::into_pointer_value)
        .ok_or_else(|| CodegenError::FailClosed("code acquisition returned no root".into()))
}

fn acquire_callable_code<'ctx>(
    ctx: &'ctx Context,
    target_data: &TargetData,
    llvm: &Module<'ctx>,
    builder: &Builder<'ctx>,
    descriptor: PointerValue<'ctx>,
) -> CodegenResult<PointerValue<'ctx>> {
    let layout = builder
        .build_load(
            ctx.ptr_type(AddressSpace::default()),
            descriptor,
            "callable.environment.layout",
        )
        .llvm_ctx("read callable code provenance")?
        .into_pointer_value();
    acquire_value_code(ctx, target_data, llvm, builder, layout)
}

pub(super) fn release_code<'ctx>(
    ctx: &'ctx Context,
    llvm: &Module<'ctx>,
    builder: &Builder<'ctx>,
    root: PointerValue<'ctx>,
) -> CodegenResult<()> {
    let release = get_or_declare_external(
        llvm,
        "hew_code_root_release",
        ctx.void_type()
            .fn_type(&[ctx.ptr_type(AddressSpace::default()).into()], false),
    )?;
    builder
        .build_call(release, &[root.into()], "")
        .llvm_ctx("release defining-code root")?;
    Ok(())
}

fn receiver_disposal_suspends(
    module: &PhysicalModule,
    receiver: &hew_mir::physical::PhysicalValueRecipe,
    output: Option<&hew_mir::physical::PhysicalValueRecipe>,
) -> bool {
    receiver.destroy.is_some_and(|action| {
        module.releases.suspends(action)
            || (module.releases.raises_fault(action)
                && output
                    .and_then(|output| output.destroy)
                    .is_some_and(|action| module.releases.suspends(action)))
    })
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
        let target = self.target_data;
        let layout = self
            .module
            .target
            .environment_layout(&glue.ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("callable environment lacks its target layout".into())
            })?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let size = self.ctx.ptr_sized_int_type(target, None);
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
        let descriptor_ty = value_descriptor_type(self.ctx, target);
        let descriptor = descriptor_ty.const_named_struct(&[
            size.const_int(layout.size, false).into(),
            size.const_int(u64::from(layout.align), false).into(),
            self.ctx.i8_type().const_int(ownership as u64, false).into(),
            clone.into(),
            drop.into(),
            if glue.fields.iter().any(|field| {
                field
                    .destroy
                    .is_some_and(|action| self.module.releases.suspends(action))
            }) {
                release::custom(
                    self.ctx,
                    self.target_data,
                    &self.llvm,
                    self.module,
                    &format!("{name}_release"),
                    |values, frame, environment| {
                        for (index, field) in glue.fields.iter().enumerate().rev() {
                            let index = index as u32;
                            let live =
                                environment_mask_bit(self.ctx, values.builder, environment, index)?;
                            let destroy =
                                self.ctx.append_basic_block(values.value, "capture.release");
                            let next = self.ctx.append_basic_block(values.value, "capture.next");
                            values
                                .builder
                                .build_conditional_branch(live, destroy, next)
                                .llvm_ctx("select retained capture")?;
                            values.builder.position_at_end(destroy);
                            set_environment_mask_bit(
                                self.ctx,
                                values.builder,
                                environment,
                                index,
                                false,
                            )?;
                            if let Some(action) = field.destroy {
                                let slot = environment_field(
                                    self.ctx,
                                    values.builder,
                                    layout,
                                    environment,
                                    index,
                                )?;
                                let field_layout =
                                    self.module.target.layout(&field.ty).ok_or_else(|| {
                                        CodegenError::FailClosed(
                                            "capture release lacks layout".into(),
                                        )
                                    })?;
                                release::slot(values, frame, slot, field_layout, action)?;
                            }
                            values
                                .builder
                                .build_unconditional_branch(next)
                                .llvm_ctx("continue capture cleanup")?;
                            values.builder.position_at_end(next);
                        }
                        Ok(())
                    },
                )?
                .as_global_value()
                .as_pointer_value()
            } else {
                pointer.const_null()
            }
            .into(),
            self.code_owner().into(),
            self.emit_environment_managed(name, glue, layout)?.into(),
        ]);
        let global = self.llvm.add_global(descriptor_ty, None, name);
        global.set_linkage(Linkage::Internal);
        global.set_constant(true);
        global.set_initializer(&descriptor);
        Ok(())
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
        // A captured `#[resource]` whose `close` fails records here, so the
        // remaining captures are still released before the fault leaves.
        let record = crate::physical::glue_fault_record(self.ctx, &builder)?;
        let emitter = ValueEmitter {
            module: self.module,
            target_data: self.target_data,
            ctx: self.ctx,
            llvm: &self.llvm,
            builder: &builder,
            value: function,
            fault_sink: Some(record),
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
                let value = emitter.load_value(slot, field_layout, "capture.owner")?;
                emitter.destroy_loaded_value(value, field_layout, action)?;
            }
            builder
                .build_unconditional_branch(next)
                .llvm_ctx("finish capture destruction")?;
            builder.position_at_end(next);
        }
        crate::physical::raise_glue_fault_record(self.ctx, &self.llvm, &builder, function, record)?;
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

pub(super) fn environment_field<'ctx>(
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

use hew_mir::physical::{ClosureId, PhysicalClosure, StorageOrigin};

pub(super) fn callable_descriptor_type(ctx: &Context) -> inkwell::types::StructType<'_> {
    let pointer = ctx.ptr_type(AddressSpace::default());
    ctx.struct_type(&[pointer.into(); 6], false)
}

pub(super) fn callable_carrier<'ctx>(
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
pub(super) fn prepared_descriptor_symbol(id: CallableId) -> String {
    format!("__hew_prepared_function_{}", id.0)
}

pub(super) fn prepared_mutable_symbol(id: CallableId, update: hew_types::ReceiverUpdate) -> String {
    format!("__hew_prepared_mutable_{}_{update:?}", id.0)
}

fn closure_descriptor_symbol(id: ClosureId) -> String {
    format!("__hew_callable_closure_{}", id.0)
}

fn closure_entry_descriptor_symbol(
    id: ClosureId,
    specialization: Option<hew_mir::physical::CallableSpecializationId>,
) -> String {
    specialization.map_or_else(
        || closure_descriptor_symbol(id),
        |entry| format!("__hew_callable_closure_{}_entry_{}", id.0, entry.0),
    )
}

fn invocation_suspends(
    module: &PhysicalModule,
    body: &PhysicalCallable,
    receiver: Option<&PhysicalEnvironmentGlue>,
    consuming: bool,
) -> bool {
    if body.is_resumable {
        return true;
    }
    let Some(receiver) = receiver.filter(|_| {
        consuming && body.params[0].passing != hew_mir::physical::SemParamPassing::Consume
    }) else {
        return false;
    };
    receiver.fields.iter().any(|field| {
        field
            .destroy
            .is_some_and(|action| module.releases.suspends(action))
    }) || (receiver.fields.iter().any(|field| {
        field
            .destroy
            .is_some_and(|action| module.releases.raises_fault(action))
    }) && body
        .return_recipe
        .as_ref()
        .and_then(|recipe| recipe.destroy)
        .is_some_and(|action| module.releases.suspends(action)))
}

impl<'ctx> ModuleEmitter<'ctx, '_> {
    pub(super) fn emit_callable_descriptors(&self) -> CodegenResult<()> {
        for closure in &self.module.closures {
            for entry in &closure.entries {
                let body = callable(self.module, entry.body)?;
                self.emit_callable_descriptor(
                    &closure_entry_descriptor_symbol(closure.id, entry.specialization),
                    body,
                    &closure.ty,
                    Some(closure),
                )?;
            }
            if matches!(closure.ty, ResolvedTy::GenericCallable { .. }) {
                self.emit_callable_family_descriptor(closure)?;
            }
        }
        let mut functions = BTreeMap::new();
        for function in &self.module.functions {
            for op in function.blocks.iter().flat_map(|block| block.operations()) {
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
        let prepared = self
            .module
            .functions
            .iter()
            .flat_map(|function| &function.blocks)
            .flat_map(|block| block.operations())
            .filter_map(|op| {
                if let PhysicalOp::TaskInvoke {
                    callee: hew_mir::physical::PhysicalInvocationCallee::Direct(target),
                    ..
                } = op
                {
                    Some(*target)
                } else {
                    None
                }
            })
            .collect::<std::collections::BTreeSet<_>>();
        let mut dynamic = Vec::new();
        for function in &self.module.functions {
            for op in function.blocks.iter().flat_map(|block| block.operations()) {
                if let PhysicalOp::TaskInvoke {
                    callee: callee @ hew_mir::physical::PhysicalInvocationCallee::Dyn { .. },
                    dest,
                    ..
                } = op
                {
                    dynamic.push((
                        super::dyn_object::prepared_dyn_symbol(function.callable, *dest),
                        callee,
                    ));
                }
            }
        }
        let mut mutable = BTreeMap::new();
        for op in self
            .module
            .functions
            .iter()
            .flat_map(|function| &function.blocks)
            .flat_map(|block| block.operations())
        {
            if let PhysicalOp::TaskInvoke {
                callee:
                    hew_mir::physical::PhysicalInvocationCallee::Mutable {
                        target,
                        update,
                        recipe,
                        ..
                    },
                output,
                ..
            } = op
            {
                mutable.insert(
                    prepared_mutable_symbol(*target, *update),
                    (*target, *update, recipe, output.as_ref()),
                );
            }
        }
        if !prepared.is_empty() || !mutable.is_empty() || !dynamic.is_empty() {
            let target = self.target_data;
            let pointer = self.ctx.ptr_type(AddressSpace::default());
            let size = self.ctx.ptr_sized_int_type(target, None);
            let ty = value_descriptor_type(self.ctx, target);
            let environment = self.llvm.add_global(ty, None, "__hew_prepared_environment");
            environment.set_linkage(Linkage::Internal);
            environment.set_constant(true);
            environment.set_initializer(
                &ty.const_named_struct(&[
                    size.const_zero().into(),
                    size.const_int(1, false).into(),
                    self.ctx
                        .i8_type()
                        .const_int(HewTypeOwnershipKind::Plain as u64, false)
                        .into(),
                    pointer.const_null().into(),
                    pointer.const_null().into(),
                    pointer.const_null().into(),
                    self.code_owner().into(),
                    pointer.const_null().into(),
                ]),
            );
            for (name, callee) in dynamic {
                self.emit_prepared_dyn_descriptor(&name, environment.as_pointer_value(), callee)?;
            }
            for (name, (id, update, recipe, output)) in mutable {
                self.emit_invocation_descriptor(
                    &name,
                    callable(self.module, id)?,
                    environment.as_pointer_value(),
                    None,
                    Some((update, recipe, output)),
                )?;
            }
            for id in prepared {
                self.emit_invocation_descriptor(
                    &prepared_descriptor_symbol(id),
                    callable(self.module, id)?,
                    environment.as_pointer_value(),
                    None,
                    None,
                )?;
            }
        }
        Ok(())
    }

    fn emit_callable_family_descriptor(&self, closure: &PhysicalClosure) -> CodegenResult<()> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let index = self
            .module
            .environment_glue
            .iter()
            .position(|glue| glue.ty == closure.ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("generic closure lacks an environment recipe".into())
            })?;
        let environment = self
            .llvm
            .get_global(&environment_descriptor_symbol(index))
            .ok_or_else(|| {
                CodegenError::FailClosed("generic closure lacks its environment layout".into())
            })?;
        let mut entries = vec![pointer.const_null(); self.module.callable_specializations.len()];
        for entry in &closure.entries {
            let id = entry.specialization.ok_or_else(|| {
                CodegenError::FailClosed("generic closure has a monomorphic entry".into())
            })?;
            let descriptor = self
                .llvm
                .get_global(&closure_entry_descriptor_symbol(closure.id, Some(id)))
                .ok_or_else(|| {
                    CodegenError::FailClosed(
                        "generic closure invocation lacks its descriptor".into(),
                    )
                })?;
            entries[id.0 as usize] = descriptor.as_pointer_value();
        }
        let table_pointer = if entries.is_empty() {
            pointer.const_null()
        } else {
            let values = pointer.const_array(&entries);
            let table = self.llvm.add_global(
                values.get_type(),
                None,
                &format!("{}_entries", closure_descriptor_symbol(closure.id)),
            );
            table.set_linkage(Linkage::Internal);
            table.set_constant(true);
            table.set_initializer(&values);
            table.as_pointer_value()
        };
        let descriptor_ty = callable_descriptor_type(self.ctx);
        let descriptor =
            self.llvm
                .add_global(descriptor_ty, None, &closure_descriptor_symbol(closure.id));
        descriptor.set_linkage(Linkage::Internal);
        descriptor.set_constant(true);
        descriptor.set_initializer(&descriptor_ty.const_named_struct(&[
            environment.as_pointer_value().into(),
            pointer.const_null().into(),
            pointer.const_null().into(),
            pointer.const_null().into(),
            pointer.const_null().into(),
            table_pointer.into(),
        ]));
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
        self.emit_invocation_descriptor(
            name,
            body,
            environment.as_pointer_value(),
            closure.map(|_| &self.module.environment_glue[index]),
            None,
        )
    }

    fn emit_invocation_descriptor(
        &self,
        name: &str,
        body: &PhysicalCallable,
        environment: PointerValue<'ctx>,
        receiver: Option<&PhysicalEnvironmentGlue>,
        mutable: Option<(
            hew_types::ReceiverUpdate,
            &hew_mir::physical::PhysicalValueRecipe,
            Option<&hew_mir::physical::PhysicalValueRecipe>,
        )>,
    ) -> CodegenResult<()> {
        let descriptor_ty = callable_descriptor_type(self.ctx);
        let descriptor = self.llvm.add_global(descriptor_ty, None, name);
        descriptor.set_linkage(Linkage::Internal);
        descriptor.set_constant(true);
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let disposal_suspends = mutable.is_some_and(|(_, recipe, output)| {
            receiver_disposal_suspends(self.module, recipe, output)
        });
        let owned_body = receiver.is_some()
            && body.params[0].passing == hew_mir::physical::SemParamPassing::Consume;
        let borrow = if owned_body {
            pointer.const_null()
        } else {
            self.emit_invoke_adapter(
                &format!("{name}_borrow"),
                descriptor.as_pointer_value(),
                body,
                receiver,
                mutable,
                false,
                false,
            )?
            .as_global_value()
            .as_pointer_value()
        };
        let once = self.emit_invoke_adapter(
            &format!("{name}_once"),
            descriptor.as_pointer_value(),
            body,
            receiver,
            mutable,
            true,
            false,
        )?;
        let synchronous_borrow = if !owned_body && !body.is_resumable && !disposal_suspends {
            self.emit_invoke_adapter(
                &format!("{name}_borrow_sync"),
                descriptor.as_pointer_value(),
                body,
                receiver,
                mutable,
                false,
                true,
            )?
            .as_global_value()
            .as_pointer_value()
        } else {
            pointer.const_null()
        };
        let synchronous_once =
            if !invocation_suspends(self.module, body, receiver, true) && !disposal_suspends {
                self.emit_invoke_adapter(
                    &format!("{name}_once_sync"),
                    descriptor.as_pointer_value(),
                    body,
                    receiver,
                    mutable,
                    true,
                    true,
                )?
                .as_global_value()
                .as_pointer_value()
            } else {
                pointer.const_null()
            };
        descriptor.set_initializer(&descriptor_ty.const_named_struct(&[
            environment.into(),
            borrow.into(),
            once.as_global_value().as_pointer_value().into(),
            synchronous_borrow.into(),
            synchronous_once.into(),
            pointer.const_null().into(),
        ]));
        Ok(())
    }

    #[allow(
        clippy::too_many_arguments,
        clippy::too_many_lines,
        reason = "receiver handback and staged cleanup complete before invocation publication"
    )]
    fn finish_prepared_receiver(
        &self,
        builder: &Builder<'ctx>,
        function: FunctionValue<'ctx>,
        frame: Option<&coro::Frame<'ctx>>,
        body: &PhysicalCallable,
        recipe: &hew_mir::physical::PhysicalValueRecipe,
        output: Option<&hew_mir::physical::PhysicalValueRecipe>,
        update: hew_types::ReceiverUpdate,
        receiver: PointerValue<'ctx>,
        dual: PointerValue<'ctx>,
        result: PointerValue<'ctx>,
        fault: PointerValue<'ctx>,
        status: IntValue<'ctx>,
    ) -> CodegenResult<IntValue<'ctx>> {
        let dual_ty =
            llvm_type(self.ctx, &body.return_layout.as_ref().unwrap().repr)?.into_struct_type();
        let receiver_layout = &body.params[0].layout;
        let returned = builder
            .build_struct_gep(dual_ty, dual, 1, "method.handback")
            .llvm_ctx("address returned receiver")?;
        let value = builder
            .build_load(
                llvm_type(self.ctx, &receiver_layout.repr)?,
                returned,
                "method.returned.receiver",
            )
            .llvm_ctx("read returned receiver")?;
        let status_slot = builder
            .build_alloca(self.ctx.i32_type(), "method.status")
            .llvm_ctx("allocate method outcome")?;
        builder
            .build_store(status_slot, status)
            .llvm_ctx("retain method outcome")?;
        let values = ValueEmitter {
            module: self.module,
            target_data: self.target_data,
            ctx: self.ctx,
            llvm: &self.llvm,
            builder,
            value: function,
            fault_sink: Some((fault, status_slot)),
        };
        let success = self.ctx.append_basic_block(function, "method.success");
        let failure = self.ctx.append_basic_block(function, "method.failure");
        let done = self.ctx.append_basic_block(function, "method.done");
        let ok = builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_zero(),
                "method.ok",
            )
            .llvm_ctx("test method result")?;
        if update == hew_types::ReceiverUpdate::Replace {
            builder
                .build_store(receiver, value)
                .llvm_ctx("restore receiver on success or fault")?;
        }
        builder
            .build_conditional_branch(ok, success, failure)
            .llvm_ctx("select receiver completion")?;
        builder.position_at_end(failure);
        if update == hew_types::ReceiverUpdate::Staged {
            if let Some(action) = recipe.destroy {
                if let Some(frame) = frame {
                    release::slot(&values, frame, returned, receiver_layout, action)?;
                } else {
                    values.destroy_loaded_value(value, receiver_layout, action)?;
                }
            }
        }
        builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish failed method handback")?;
        builder.position_at_end(success);
        if update == hew_types::ReceiverUpdate::Staged {
            let previous = builder
                .build_load(
                    llvm_type(self.ctx, &receiver_layout.repr)?,
                    receiver,
                    "method.previous.receiver",
                )
                .llvm_ctx("read displaced receiver")?;
            builder
                .build_store(receiver, value)
                .llvm_ctx("commit staged receiver")?;
            if let Some(action) = recipe.destroy {
                if let Some(frame) = frame {
                    builder
                        .build_store(returned, previous)
                        .llvm_ctx("stage displaced receiver cleanup")?;
                    release::slot(&values, frame, returned, receiver_layout, action)?;
                } else {
                    values.destroy_loaded_value(previous, receiver_layout, action)?;
                }
            }
        }
        if let Some(output) = output.filter(|output| output.ty != ResolvedTy::Unit) {
            let PhysicalRepr::Struct(fields) = &body.return_layout.as_ref().unwrap().repr else {
                unreachable!("verified mutable method returns its user value and receiver")
            };
            let result_slot = builder
                .build_struct_gep(dual_ty, dual, 0, "method.user.result")
                .llvm_ctx("address method user result")?;
            let combined = builder
                .build_load(self.ctx.i32_type(), status_slot, "method.combined.status")
                .llvm_ctx("read receiver cleanup outcome")?
                .into_int_value();
            let publish = self.ctx.append_basic_block(function, "method.publish");
            let discard = self.ctx.append_basic_block(function, "method.discard");
            let ok = builder
                .build_int_compare(
                    IntPredicate::EQ,
                    combined,
                    self.ctx.i32_type().const_zero(),
                    "method.cleanup.ok",
                )
                .llvm_ctx("test receiver cleanup")?;
            builder
                .build_conditional_branch(ok, publish, discard)
                .llvm_ctx("retain result only on success")?;
            builder.position_at_end(publish);
            let value = builder
                .build_load(
                    llvm_type(self.ctx, &fields[0].repr)?,
                    result_slot,
                    "method.result",
                )
                .llvm_ctx("read method result")?;
            builder
                .build_store(result, value)
                .llvm_ctx("publish method result")?;
            builder
                .build_unconditional_branch(done)
                .llvm_ctx("finish method publication")?;
            builder.position_at_end(discard);
            if let Some(action) = output.destroy {
                if let Some(frame) = frame {
                    release::slot(&values, frame, result_slot, &fields[0], action)?;
                } else {
                    let value = builder
                        .build_load(
                            llvm_type(self.ctx, &fields[0].repr)?,
                            result_slot,
                            "method.abandoned.result",
                        )
                        .llvm_ctx("read abandoned method result")?;
                    values.destroy_loaded_value(value, &fields[0], action)?;
                }
            }
        }
        builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish method result cleanup")?;
        builder.position_at_end(done);
        Ok(builder
            .build_load(self.ctx.i32_type(), status_slot, "method.final.status")
            .llvm_ctx("read final method outcome")?
            .into_int_value())
    }

    #[allow(
        clippy::too_many_arguments,
        reason = "one descriptor adapter retains receiver recipes and both ABI entry modes"
    )]
    fn emit_invoke_adapter(
        &self,
        name: &str,
        descriptor: PointerValue<'ctx>,
        body: &PhysicalCallable,
        receiver_recipe: Option<&PhysicalEnvironmentGlue>,
        mutable: Option<(
            hew_types::ReceiverUpdate,
            &hew_mir::physical::PhysicalValueRecipe,
            Option<&hew_mir::physical::PhysicalValueRecipe>,
        )>,
        consuming: bool,
        synchronous: bool,
    ) -> CodegenResult<FunctionValue<'ctx>> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let function = self.llvm.add_function(
            name,
            if synchronous {
                self.ctx.i32_type().fn_type(&[pointer.into(); 4], false)
            } else {
                pointer.fn_type(&[pointer.into(); 5], false)
            },
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(function, "entry"));
        let environment = function.get_nth_param(0).unwrap().into_pointer_value();
        let slots = function.get_nth_param(1).unwrap().into_pointer_value();
        let result_out = function.get_nth_param(2).unwrap().into_pointer_value();
        let fault_out = function.get_nth_param(3).unwrap().into_pointer_value();
        let state = function
            .get_nth_param(4)
            .map_or_else(|| pointer.const_null(), BasicValueEnum::into_pointer_value);
        let has_receiver = receiver_recipe.is_some();
        let weakened_once = consuming
            && has_receiver
            && body.params[0].passing != hew_mir::physical::SemParamPassing::Consume;
        // Consuming bodies copy their receiver and ordinary arguments into
        // entry storage before suspension, then own all cleanup. Their adapters
        // can transfer that frame directly instead of allocating a second one.
        let transfers_receiver = has_receiver
            && consuming
            && body.params[0].passing == hew_mir::physical::SemParamPassing::Consume
            && body.params.iter().all(|parameter| {
                parameter.passing != hew_mir::physical::SemParamPassing::BorrowMut
            });
        let frame = if transfers_receiver
            || (!invocation_suspends(self.module, body, receiver_recipe, consuming)
                && !mutable.is_some_and(|(_, recipe, output)| {
                    receiver_disposal_suspends(self.module, recipe, output)
                })) {
            None
        } else {
            Some(coro::begin(
                self.ctx,
                self.target_data,
                &self.llvm,
                &builder,
                function,
                state,
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
        if let Some((update, recipe, _)) = mutable {
            let parameter = &body.params[0];
            let mut value = builder
                .build_load(
                    llvm_type(self.ctx, &parameter.layout.repr)?,
                    environment,
                    "method.receiver",
                )
                .llvm_ctx("read prepared mutable receiver")?;
            if update == hew_types::ReceiverUpdate::Staged {
                let values = ValueEmitter {
                    module: self.module,
                    target_data: self.target_data,
                    ctx: self.ctx,
                    llvm: &self.llvm,
                    builder: &builder,
                    value: function,
                    fault_sink: None,
                };
                value = values.clone_loaded_value(
                    value,
                    &parameter.layout,
                    recipe.clone.ok_or_else(|| {
                        CodegenError::FailClosed("staged receiver lacks its copy recipe".into())
                    })?,
                )?;
            }
            arguments.push(match parameter.carrier {
                ParamCarrier::Direct => value.into(),
                ParamCarrier::Indirect => {
                    let slot = builder
                        .build_alloca(value.get_type(), "method.receiver.input")
                        .llvm_ctx("stage prepared receiver")?;
                    builder
                        .build_store(slot, value)
                        .llvm_ctx("initialize prepared receiver")?;
                    slot.into()
                }
            });
        }
        for (index, parameter) in body
            .params
            .iter()
            .skip(usize::from(has_receiver || mutable.is_some()))
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
        let dual_result = mutable
            .map(|_| {
                builder
                    .build_alloca(
                        llvm_type(self.ctx, &body.return_layout.as_ref().unwrap().repr)?,
                        "method.dual.result",
                    )
                    .llvm_ctx("allocate prepared receiver handback")
            })
            .transpose()?;
        let staged_result = if weakened_once {
            body.return_layout
                .as_ref()
                .map(|layout| {
                    builder
                        .build_alloca(llvm_type(self.ctx, &layout.repr)?, "invoke.result.pending")
                        .llvm_ctx("stage result until receiver cleanup completes")
                })
                .transpose()?
        } else {
            None
        };
        if body.return_layout.is_some() {
            arguments.push(dual_result.or(staged_result).unwrap_or(result_out).into());
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
            let child = frame
                .as_ref()
                .expect("borrowed adapter owns a frame")
                .init_child(self.ctx, &self.llvm, &builder)?;
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
        let status = if let Some((update, recipe, output)) = mutable {
            self.finish_prepared_receiver(
                &builder,
                function,
                frame.as_ref(),
                body,
                recipe,
                output,
                update,
                environment,
                dual_result.unwrap(),
                result_out,
                fault_out,
                status,
            )?
        } else {
            status
        };
        // A weakened borrowed body leaves receiver disposal to this adapter.
        // Keep a successful result private until that release also succeeds.
        let status = if weakened_once {
            let status_slot = builder
                .build_alloca(self.ctx.i32_type(), "invoke.status.slot")
                .llvm_ctx("allocate combined invocation status")?;
            builder
                .build_store(status_slot, status)
                .llvm_ctx("retain body outcome")?;
            let values = ValueEmitter {
                module: self.module,
                target_data: self.target_data,
                ctx: self.ctx,
                llvm: &self.llvm,
                builder: &builder,
                value: function,
                fault_sink: Some((fault_out, status_slot)),
            };
            let result_initialized = if staged_result.is_some() {
                let initialized = builder
                    .build_alloca(self.ctx.bool_type(), "invoke.result.live")
                    .llvm_ctx("allocate staged result state")?;
                let succeeded = builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        status,
                        self.ctx.i32_type().const_zero(),
                        "invoke.body.succeeded",
                    )
                    .llvm_ctx("test whether the body initialized a result")?;
                builder
                    .build_store(initialized, succeeded)
                    .llvm_ctx("retain result ownership across receiver release")?;
                Some(initialized)
            } else {
                None
            };
            let receiver = receiver.unwrap();
            if let Some(frame) = &frame {
                release::slot(
                    &values,
                    frame,
                    receiver,
                    &body.params[0].layout,
                    DestroyAction::Callable,
                )?;
            } else {
                let drop = external_drop(self.ctx, &self.llvm, "hew_callable_drop")?;
                values.emit_release_in_sink(|| {
                    builder
                        .build_call(drop, &[receiver.into()], "")
                        .llvm_ctx("dispose weakened once receiver on either outcome")?;
                    Ok(())
                })?;
            }
            if let Some(result_slot) = staged_result {
                let body_succeeded = builder
                    .build_load(
                        self.ctx.bool_type(),
                        result_initialized.unwrap(),
                        "invoke.result.initialized",
                    )
                    .llvm_ctx("load staged result ownership")?
                    .into_int_value();
                let initialized = self
                    .ctx
                    .append_basic_block(function, "invoke.result.initialized");
                let done = self.ctx.append_basic_block(function, "invoke.result.done");
                builder
                    .build_conditional_branch(body_succeeded, initialized, done)
                    .llvm_ctx("select initialized invocation result")?;
                builder.position_at_end(initialized);
                let combined = builder
                    .build_load(self.ctx.i32_type(), status_slot, "invoke.combined.status")
                    .llvm_ctx("load invocation cleanup outcome")?
                    .into_int_value();
                let succeeded = builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        combined,
                        self.ctx.i32_type().const_zero(),
                        "invoke.succeeded",
                    )
                    .llvm_ctx("test invocation completion")?;
                let publish = self
                    .ctx
                    .append_basic_block(function, "invoke.result.publish");
                let discard = self
                    .ctx
                    .append_basic_block(function, "invoke.result.discard");
                builder
                    .build_conditional_branch(succeeded, publish, discard)
                    .llvm_ctx("publish result only after successful cleanup")?;
                builder.position_at_end(publish);
                let layout = body.return_layout.as_ref().unwrap();
                let result = builder
                    .build_load(
                        llvm_type(self.ctx, &layout.repr)?,
                        result_slot,
                        "invoke.result",
                    )
                    .llvm_ctx("load completed invocation result")?;
                builder
                    .build_store(result_out, result)
                    .llvm_ctx("publish completed invocation result")?;
                builder
                    .build_unconditional_branch(done)
                    .llvm_ctx("finish result publication")?;
                builder.position_at_end(discard);
                if let Some(action) = body
                    .return_recipe
                    .as_ref()
                    .and_then(|recipe| recipe.destroy)
                {
                    if let Some(frame) = &frame {
                        release::slot(&values, frame, result_slot, layout, action)?;
                    } else {
                        let result = builder
                            .build_load(
                                llvm_type(self.ctx, &layout.repr)?,
                                result_slot,
                                "invoke.result.abandoned",
                            )
                            .llvm_ctx("load abandoned invocation result")?;
                        values.destroy_loaded_value(result, layout, action)?;
                    }
                }
                builder
                    .build_unconditional_branch(done)
                    .llvm_ctx("finish abandoned result cleanup")?;
                builder.position_at_end(done);
            }
            builder
                .build_load(self.ctx.i32_type(), status_slot, "invoke.final.status")
                .llvm_ctx("load combined invocation outcome")?
                .into_int_value()
        } else {
            status
        };
        if consuming && !has_receiver && mutable.is_none() {
            let carrier = callable_carrier(self.ctx, &builder, environment, descriptor)?;
            let slot = builder
                .build_alloca(carrier.get_type(), "function.consumed")
                .llvm_ctx("stage consumed function carrier")?;
            builder
                .build_store(slot, carrier)
                .llvm_ctx("transfer consumed function carrier")?;
            let drop = external_drop(self.ctx, &self.llvm, "hew_callable_drop")?;
            builder
                .build_call(drop, &[slot.into()], "")
                .llvm_ctx("release consumed function code")?;
        }
        if synchronous {
            builder
                .build_return(Some(&status))
                .llvm_ctx("return synchronous invocation status")?;
            return Ok(function);
        }
        let finish = get_or_declare_external(
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
                .llvm_ctx("finish synchronous callable")?;
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
    pub(super) fn borrowed_slot(&self, source: StorageId) -> CodegenResult<PointerValue<'ctx>> {
        let Some(parent) = self.storage(source)?.borrow_parent else {
            return Ok(self.slots[source.0 as usize]);
        };
        if self.storage(source)?.ty == self.storage(parent)?.ty {
            return self.borrowed_slot(parent);
        }
        let projection = self
            .function
            .blocks
            .iter()
            .flat_map(|block| block.operations())
            .find_map(|op| {
                if let PhysicalOp::AggregateProjectBorrow {
                    dest,
                    aggregate,
                    field,
                    glue,
                } = op
                {
                    (*dest == source && *aggregate == parent).then_some((*field, *glue))
                } else {
                    None
                }
            });
        if let Some((field, glue)) = projection {
            let recipe = &self.module.aggregate_glue[glue.0 as usize];
            let layout = self.module.target.layout(&recipe.ty).ok_or_else(|| {
                CodegenError::FailClosed("borrowed field lacks its checked layout".into())
            })?;
            return self
                .builder
                .build_struct_gep(
                    llvm_type(self.ctx, &layout.repr)?.into_struct_type(),
                    self.borrowed_slot(parent)?,
                    field,
                    "borrowed.field.address",
                )
                .llvm_ctx("address the canonical borrowed field");
        }
        Ok(self.slots[source.0 as usize])
    }

    pub(super) fn select_callable_descriptor(
        &self,
        descriptor: PointerValue<'ctx>,
        specialization: Option<hew_mir::physical::CallableSpecializationId>,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let Some(id) = specialization else {
            return Ok(descriptor);
        };
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let table_slot = self
            .builder
            .build_struct_gep(
                callable_descriptor_type(self.ctx),
                descriptor,
                5,
                "callable.specializations",
            )
            .llvm_ctx("select callable specialization table")?;
        let table = self
            .builder
            .build_load(pointer, table_slot, "callable.specialization.table")
            .llvm_ctx("load callable specialization table")?
            .into_pointer_value();
        // SAFETY: SIR selected this index in the module's descriptor table.
        let slot = unsafe {
            self.builder.build_in_bounds_gep(
                pointer,
                table,
                &[self.ctx.i32_type().const_int(u64::from(id.0), false)],
                "callable.specialization.slot",
            )
        }
        .llvm_ctx("select concrete callable entry")?;
        Ok(self
            .builder
            .build_load(pointer, slot, "callable.specialization")
            .llvm_ctx("load concrete callable descriptor")?
            .into_pointer_value())
    }

    pub(super) fn emit_callable_coerce(
        &self,
        dest: StorageId,
        source: StorageId,
        specialization: Option<hew_mir::physical::CallableSpecializationId>,
    ) -> CodegenResult<()> {
        let mut carrier = self.load(source, "callable.coerce")?.into_struct_value();
        if specialization.is_some() {
            let descriptor = self
                .builder
                .build_extract_value(carrier, 1, "callable.family")
                .llvm_ctx("load generic callable descriptor")?
                .into_pointer_value();
            let selected = self.select_callable_descriptor(descriptor, specialization)?;
            carrier = self
                .builder
                .build_insert_value(carrier, selected, 1, "callable.specialized")
                .llvm_ctx("select invocation while preserving environment")?
                .into_struct_value();
        }
        self.store(dest, carrier.into())?;
        self.clear_owned(source)
    }

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
        acquire_callable_code(
            self.ctx,
            self.target_data,
            self.llvm,
            &self.builder,
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
            self.value_emitter()
                .store_value(destination, &self.storage(*field)?.layout, source)?;
            self.clear_owned(*field)?;
            set_environment_mask_bit(self.ctx, &self.builder, environment, index, true)?;
        }
        acquire_callable_code(
            self.ctx,
            self.target_data,
            self.llvm,
            &self.builder,
            descriptor.as_pointer_value(),
        )?;
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

    pub(super) fn prepare_callable_arguments(
        &self,
        args: &[ArgumentTransfer],
    ) -> CodegenResult<(PointerValue<'ctx>, Vec<StorageId>)> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let mut moved = Vec::new();
        let argument_slots = if args.is_empty() {
            pointer.const_null()
        } else {
            let count = u32::try_from(args.len())
                .map_err(|_| CodegenError::FailClosed("argument array exceeds u32".into()))?;
            let slots = self
                .value_emitter()
                .entry_scratch(pointer.array_type(count).into(), "callable.arguments")?;
            for (index, argument) in args.iter().enumerate() {
                let source = argument_source(argument);
                let address = match argument {
                    ArgumentTransfer::Clone { .. } | ArgumentTransfer::Move(_) => {
                        let value = if let ArgumentTransfer::Clone { action, .. } = argument {
                            self.clone_value(source, *action)?
                        } else {
                            moved.push(source);
                            self.load(source, "callable.argument.move")?
                        };
                        let slot = self.value_emitter().entry_scratch(
                            llvm_type(
                                self.ctx,
                                &self.function.storage[source.0 as usize].layout.repr,
                            )?,
                            "callable.argument.value",
                        )?;
                        self.builder
                            .build_store(slot, value)
                            .llvm_ctx("stage owned invocation argument")?;
                        slot
                    }
                    _ => self.borrowed_slot(source)?,
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
        Ok((argument_slots, moved))
    }

    pub(super) fn emit_indirect_call(&self, call: &PhysicalTerminator) -> CodegenResult<()> {
        let PhysicalTerminator::IndirectCall {
            targets,
            specialization,
            callee,
            signature: _,
            args,
            result,
            normal,
            unwind,
        } = call
        else {
            return Err(CodegenError::FailClosed(
                "indirect emitter received another terminator".into(),
            ));
        };
        let callee = *callee;
        let result = *result;
        let normal = normal.as_ref();
        let unwind = unwind.as_ref();
        let source = argument_source(&callee);
        let carrier = self.load(source, "callable.receiver")?.into_struct_value();
        let environment = self
            .builder
            .build_extract_value(carrier, 0, "callable.environment")
            .llvm_ctx("load callable environment")?
            .into_pointer_value();
        let loaded_descriptor = self
            .builder
            .build_extract_value(carrier, 1, "callable.descriptor")
            .llvm_ctx("load callable descriptor")?
            .into_pointer_value();
        let consuming = matches!(callee, ArgumentTransfer::Move(_));
        let descriptor = if !targets.may_be_unknown && targets.known.len() == 1 {
            let target = targets.known[0];
            let name = self
                .module
                .closures
                .iter()
                .find_map(|closure| {
                    closure
                        .entries
                        .iter()
                        .find(|entry| entry.body == target)
                        .map(|entry| {
                            closure_entry_descriptor_symbol(closure.id, entry.specialization)
                        })
                })
                .unwrap_or_else(|| function_descriptor_symbol(target));
            self.llvm
                .get_global(&name)
                .ok_or_else(|| {
                    CodegenError::FailClosed("checked callable target has no descriptor".into())
                })?
                .as_pointer_value()
        } else {
            self.select_callable_descriptor(loaded_descriptor, *specialization)?
        };
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
        let (argument_slots, moved) = self.prepare_callable_arguments(args)?;
        // A borrowed receiver keeps its carrier, and the carrier's code root,
        // live for the whole call. Only a consuming call gives the carrier to
        // the callee, which may drop it before its own code finishes running.
        let code = if consuming {
            let code = acquire_callable_code(
                self.ctx,
                self.target_data,
                self.llvm,
                &self.builder,
                descriptor,
            )?;
            self.clear_owned(source)?;
            Some(code)
        } else {
            None
        };
        self.builder
            .build_store(self.active_fault, pointer.const_null())
            .llvm_ctx("clear invocation fault")?;
        let result_address =
            result.map_or_else(|| pointer.const_null(), |id| self.slots[id.0 as usize]);
        for source in &moved {
            self.clear_owned(*source)?;
        }
        let sync_slot = self
            .builder
            .build_struct_gep(
                callable_descriptor_type(self.ctx),
                descriptor,
                if consuming { 4 } else { 3 },
                "callable.sync.slot",
            )
            .llvm_ctx("select synchronous invocation entry")?;
        let sync_invoke = self
            .builder
            .build_load(pointer, sync_slot, "callable.sync.entry")
            .llvm_ctx("load synchronous invocation entry")?
            .into_pointer_value();
        let mut may_suspend = targets.may_be_unknown || consuming;
        for id in &targets.known {
            may_suspend |= callable(self.module, *id)?.is_resumable;
        }
        if !may_suspend {
            let status = self
                .builder
                .build_indirect_call(
                    self.ctx.i32_type().fn_type(&[pointer.into(); 4], false),
                    sync_invoke,
                    &[
                        environment.into(),
                        argument_slots.into(),
                        result_address.into(),
                        self.active_fault.into(),
                    ],
                    "invoke.sync.status",
                )
                .llvm_ctx("invoke checked synchronous callable")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("synchronous invocation returned no status".into())
                })?
                .into_int_value();
            self.builder
                .build_store(self.active_status, status)
                .llvm_ctx("retain invocation status")?;
            if let Some(code) = code {
                release_code(self.ctx, self.llvm, &self.builder, code)?;
            }
            return self.emit_call_outcome(status, result, normal, unwind);
        }
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("indirect invocation requires a resumable caller".into())
        })?;
        let synchronous = self
            .ctx
            .append_basic_block(self.value, "invoke.synchronous");
        let resumable = self.ctx.append_basic_block(self.value, "invoke.resumable");
        let complete = self.ctx.append_basic_block(self.value, "invoke.complete");
        let is_sync = self
            .builder
            .build_is_not_null(sync_invoke, "invoke.is_sync")
            .llvm_ctx("test proven synchronous invocation")?;
        self.builder
            .build_conditional_branch(is_sync, synchronous, resumable)
            .llvm_ctx("select invocation completion ABI")?;
        self.builder.position_at_end(synchronous);
        let sync_status = self
            .builder
            .build_indirect_call(
                self.ctx.i32_type().fn_type(&[pointer.into(); 4], false),
                sync_invoke,
                &[
                    environment.into(),
                    argument_slots.into(),
                    result_address.into(),
                    self.active_fault.into(),
                ],
                "invoke.sync.status",
            )
            .llvm_ctx("invoke synchronous callable")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed("synchronous invocation returned no status".into())
            })?
            .into_int_value();
        self.builder
            .build_unconditional_branch(complete)
            .llvm_ctx("finish synchronous invocation")?;
        self.builder.position_at_end(resumable);
        let child = frame.init_child(self.ctx, self.llvm, &self.builder)?;
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
        let status = suspend::await_child(
            self.ctx,
            self.llvm,
            &self.builder,
            self.value,
            frame,
            child,
            child_frame,
        )?;
        let resumed = self.builder.get_insert_block().unwrap();
        self.builder
            .build_unconditional_branch(complete)
            .llvm_ctx("finish resumed invocation")?;
        self.builder.position_at_end(complete);
        let outcome = self
            .builder
            .build_phi(self.ctx.i32_type(), "invoke.status")
            .llvm_ctx("merge invocation outcome")?;
        outcome.add_incoming(&[(&sync_status, synchronous), (&status, resumed)]);
        let status = outcome.as_basic_value().into_int_value();
        self.builder
            .build_store(self.active_status, status)
            .llvm_ctx("retain invocation status")?;
        if let Some(code) = code {
            release_code(self.ctx, self.llvm, &self.builder, code)?;
        }
        self.emit_call_outcome(status, result, normal, unwind)
    }
}
