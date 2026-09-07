//! Cooperative child selection from the ordinary physical destruction recipe.

use super::*;

impl<'ctx> ModuleEmitter<'ctx, '_> {
    pub(super) fn emit_value_close_callback(
        &self,
        name: &str,
        layout: &PhysicalLayout,
        action: Option<DestroyAction>,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let Some(action) = action else {
            return Ok(pointer.const_null());
        };
        if matches!(
            action,
            DestroyAction::StringRelease | DestroyAction::BytesRelease | DestroyAction::Encoding(_)
        ) {
            return Ok(pointer.const_null());
        }
        let function = self.llvm.add_function(
            name,
            self.ctx.void_type().fn_type(&[pointer.into(); 2], false),
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(function, "entry"));
        let emitter = ValueEmitter {
            module: self.module,
            ctx: self.ctx,
            llvm: &self.llvm,
            builder: &builder,
            value: function,
        };
        emitter.visit_close(
            function.get_nth_param(0).unwrap().into_pointer_value(),
            layout,
            action,
            function.get_nth_param(1).unwrap().into_pointer_value(),
        )?;
        builder
            .build_return(None)
            .llvm_ctx("finish child selection")?;
        Ok(function.as_global_value().as_pointer_value())
    }
}

impl<'ctx> ValueEmitter<'_, 'ctx> {
    pub(super) fn visit_close(
        &self,
        slot: PointerValue<'ctx>,
        layout: &PhysicalLayout,
        action: DestroyAction,
        context: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        match action {
            DestroyAction::Resource(id) => {
                // A record resource releases through its own `close` body,
                // which is an ordinary synchronous call with no cooperative
                // child of its own.
                if matches!(
                    self.module.resources[id.0 as usize].release,
                    hew_mir::physical::ResourceRelease::RecordClose { .. }
                ) {
                    return Ok(());
                }
                if self.module.resources[id.0 as usize]
                    .release
                    .runtime_family()
                    .map_err(CodegenError::FailClosed)?
                    != hew_types::RuntimeCallFamily::GeneratorFree
                {
                    return Ok(());
                }
                let handle = self
                    .builder
                    .build_load(pointer, slot, "close.generator")
                    .llvm_ctx("borrow generator child")?;
                let callback = coro::external(
                    self.llvm,
                    "hew_checked_generator_close_poll",
                    self.ctx.i32_type().fn_type(&[pointer.into(); 3], false),
                )?;
                let push = coro::external(
                    self.llvm,
                    "hew_value_close_push",
                    self.ctx.void_type().fn_type(&[pointer.into(); 3], false),
                )?;
                self.builder
                    .build_call(
                        push,
                        &[
                            context.into(),
                            handle.into(),
                            callback.as_global_value().as_pointer_value().into(),
                        ],
                        "",
                    )
                    .llvm_ctx("select generator cleanup")?;
            }
            DestroyAction::Callable
            | DestroyAction::Vector(_)
            | DestroyAction::Map(_)
            | DestroyAction::Set(_) => {
                let (symbol, owner) = if action == DestroyAction::Callable {
                    (
                        "hew_callable_visit_close",
                        BasicValueEnum::PointerValue(slot),
                    )
                } else {
                    let symbol = match action {
                        DestroyAction::Vector(_) => "hew_vec_visit_close",
                        DestroyAction::Map(_) => "hew_hashmap_visit_close",
                        _ => "hew_hashset_visit_close",
                    };
                    (
                        symbol,
                        self.builder
                            .build_load(pointer, slot, "close.collection")
                            .llvm_ctx("borrow collection")?,
                    )
                };
                let visit = coro::external(
                    self.llvm,
                    symbol,
                    self.ctx.void_type().fn_type(&[pointer.into(); 2], false),
                )?;
                self.builder
                    .build_call(visit, &[owner.into(), context.into()], "")
                    .llvm_ctx("select nested owners")?;
            }
            DestroyAction::Aggregate(id) => {
                let glue = self.aggregate_glue(id)?;
                let object = llvm_type(self.ctx, &layout.repr)?.into_struct_type();
                self.visit_close_fields(slot, object, &glue.fields, context)?;
            }
            DestroyAction::Variant(id) => {
                let glue = self.variant_glue(id)?;
                let variant = self.variant_layout(&glue.ty)?;
                if variant.is_indirect {
                    return Err(CodegenError::FailClosed(
                        "indirect variant cleanup lacks admitted destruction".into(),
                    ));
                }
                let object = llvm_type(self.ctx, &variant.object.repr)?.into_struct_type();
                let tag_slot = self
                    .builder
                    .build_struct_gep(object, slot, 0, "close.tag.slot")
                    .llvm_ctx("find active variant tag")?;
                let tag = self
                    .builder
                    .build_load(
                        object.get_field_type_at_index(0).unwrap(),
                        tag_slot,
                        "close.tag",
                    )
                    .llvm_ctx("read active variant tag")?
                    .into_int_value();
                let invalid = self
                    .ctx
                    .append_basic_block(self.value, "close.variant.invalid");
                let done = self
                    .ctx
                    .append_basic_block(self.value, "close.variant.done");
                let cases = glue
                    .variants
                    .iter()
                    .enumerate()
                    .map(|(index, _)| {
                        (
                            tag.get_type().const_int(index as u64, false),
                            self.ctx
                                .append_basic_block(self.value, "close.variant.case"),
                        )
                    })
                    .collect::<Vec<_>>();
                self.builder
                    .build_switch(tag, invalid, &cases)
                    .llvm_ctx("select active variant cleanup")?;
                for (index, (_, block)) in cases.iter().enumerate() {
                    self.builder.position_at_end(*block);
                    if !glue.variants[index].fields.is_empty() {
                        let payload = self.variant_payload_ptr(slot, variant)?;
                        let object =
                            llvm_type(self.ctx, &variant.variants[index].repr)?.into_struct_type();
                        self.visit_close_fields(
                            payload,
                            object,
                            &glue.variants[index].fields,
                            context,
                        )?;
                    }
                    self.builder
                        .build_unconditional_branch(done)
                        .llvm_ctx("finish active variant cleanup")?;
                }
                self.builder.position_at_end(invalid);
                self.emit_invalid_variant_tag()?;
                self.builder.position_at_end(done);
            }
            DestroyAction::StringRelease
            | DestroyAction::BytesRelease
            | DestroyAction::Encoding(_) => {}
        }
        Ok(())
    }

    fn visit_close_fields(
        &self,
        slot: PointerValue<'ctx>,
        object: inkwell::types::StructType<'ctx>,
        fields: &[PhysicalValueRecipe],
        context: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        for (index, field) in fields.iter().enumerate().rev() {
            if let Some(action) = field.destroy {
                let index = u32::try_from(index).map_err(|_| {
                    CodegenError::FailClosed("cleanup field index exceeds u32".into())
                })?;
                let field_slot = self
                    .builder
                    .build_struct_gep(object, slot, index, "close.field")
                    .llvm_ctx("borrow initialized field")?;
                let layout = self.module.target.layout(&field.ty).ok_or_else(|| {
                    CodegenError::FailClosed("cleanup field has no layout".into())
                })?;
                self.visit_close(field_slot, layout, action, context)?;
            }
        }
        Ok(())
    }
}
