//! Runtime buffers enter through the same typed message layout and destructor
//! as source submissions. A refused submission releases the copied field here.

use super::*;

impl<'ctx> ModuleEmitter<'ctx, '_> {
    pub(in super::super) fn emit_actor_ingress_adapters(&self) -> CodegenResult<()> {
        let mut emitted = std::collections::HashSet::new();
        for function in &self.module.functions {
            for operation in function.blocks.iter().flat_map(|block| &block.ops) {
                if let PhysicalOp::Const {
                    value: PhysicalConst::ActorIngressAdapter(adapter),
                    ..
                } = operation
                {
                    if emitted.insert(*adapter) {
                        self.emit_actor_ingress_adapter(*adapter)?;
                    }
                }
            }
        }
        Ok(())
    }

    #[allow(
        clippy::too_many_lines,
        reason = "message construction, admission and refused-owner cleanup form one callback boundary"
    )]
    fn emit_actor_ingress_adapter(&self, adapter: ActorIngressAdapter) -> CodegenResult<()> {
        let handler = adapter
            .handler(&self.module.actors)
            .map_err(CodegenError::FailClosed)?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let target = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let i32_ty = self.ctx.i32_type();
        let function = self.llvm.add_function(
            &ingress_symbol(adapter),
            i32_ty.fn_type(&[size_ty.into(), ptr.into(), size_ty.into()], false),
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(function, "entry"));
        let pid = function.get_nth_param(0).unwrap();
        let data = function.get_nth_param(1).unwrap();
        let length = function.get_nth_param(2).unwrap().into_int_value();
        let refused = self.ctx.append_basic_block(function, "refused");
        let allocate_block = self.ctx.append_basic_block(function, "allocate");
        let too_long = builder
            .build_int_compare(
                IntPredicate::UGT,
                length,
                size_ty.const_int(u64::from(u32::MAX), false),
                "ingress.too_long",
            )
            .llvm_ctx("check ingress buffer length")?;
        builder
            .build_conditional_branch(too_long, refused, allocate_block)
            .llvm_ctx("admit representable ingress buffer")?;
        builder.position_at_end(refused);
        builder
            .build_return(Some(&i32_ty.const_int(3, false)))
            .llvm_ctx("return ingress allocation refusal")?;
        builder.position_at_end(allocate_block);
        let wrapper_ty = message_type(self.module, self.ctx, handler)?;
        let size = target.get_abi_size(&wrapper_ty);
        let allocator = get_or_declare_external(
            &self.llvm,
            "hew_actor_payload_try_alloc",
            ptr.fn_type(&[size_ty.into()], false),
        )?;
        let wrapper = call_value(
            &builder,
            allocator,
            &[size_ty.const_int(size, false).into()],
            "ingress.wrapper",
        )?
        .into_pointer_value();
        let allocated = self.ctx.append_basic_block(function, "allocated");
        let missing = builder
            .build_is_null(wrapper, "ingress.no_memory")
            .llvm_ctx("check ingress allocation")?;
        builder
            .build_conditional_branch(missing, refused, allocated)
            .llvm_ctx("admit allocated ingress wrapper")?;
        builder.position_at_end(allocated);
        builder
            .build_store(wrapper, self.ctx.i8_type().const_int(1, false))
            .llvm_ctx("initialize ingress payload ownership")?;
        let payload = if let Some(ty) = handler.params.first() {
            let slot = builder
                .build_struct_gep(wrapper_ty, wrapper, 1, "ingress.field")
                .llvm_ctx("address ingress payload")?;
            let symbol = match ty {
                ResolvedTy::Bytes => "hew_bytes_literal_new",
                // Text producers validate UTF-8 before invoking this adapter.
                ResolvedTy::String => "hew_string_literal_new",
                _ => {
                    return Err(CodegenError::FailClosed(
                        "unsupported actor ingress payload".into(),
                    ))
                }
            };
            let constructor = get_or_declare_external(
                &self.llvm,
                symbol,
                self.ctx
                    .void_type()
                    .fn_type(&[ptr.into(), i32_ty.into(), ptr.into()], false),
            )?;
            let length = builder
                .build_int_cast(length, i32_ty, "ingress.length")
                .llvm_ctx("realize checked ingress length")?;
            builder
                .build_call(constructor, &[data.into(), length.into(), slot.into()], "")
                .llvm_ctx("copy borrowed ingress bytes")?;
            let layout =
                self.module.target.layout(ty).ok_or_else(|| {
                    CodegenError::FailClosed("ingress payload lacks layout".into())
                })?;
            let value = builder
                .build_load(llvm_type(self.ctx, &layout.repr)?, slot, "ingress.owner")
                .llvm_ctx("preserve unpublished ingress owner")?;
            Some((ty, layout, value))
        } else {
            None
        };
        let drop = self
            .llvm
            .get_function(&message_symbol(adapter.actor, adapter.message))
            .ok_or_else(|| {
                CodegenError::FailClosed("ingress lacks its exact message destructor".into())
            })?;
        let mut parameters = vec![
            size_ty.into(),
            i32_ty.into(),
            ptr.into(),
            size_ty.into(),
            ptr.into(),
        ];
        let mut arguments = vec![
            pid.into(),
            i32_ty.const_int(u64::from(adapter.message), false).into(),
            wrapper.into(),
            size_ty.const_int(size, false).into(),
            drop.as_global_value().as_pointer_value().into(),
        ];
        let symbol = if handler.params.is_empty() {
            "hew_actor_submit_native_terminal"
        } else {
            parameters.push(i32_ty.into());
            arguments.push(i32_ty.const_zero().into());
            "hew_actor_submit_native"
        };
        let submit =
            get_or_declare_external(&self.llvm, symbol, i32_ty.fn_type(&parameters, false))?;
        let status = call_value(&builder, submit, &arguments, "ingress.status")?.into_int_value();
        if let Some((ty, layout, value)) = payload {
            let rejected = self.ctx.append_basic_block(function, "rejected");
            let done = self.ctx.append_basic_block(function, "done");
            let accepted = builder
                .build_int_compare(
                    IntPredicate::EQ,
                    status,
                    i32_ty.const_zero(),
                    "ingress.accepted",
                )
                .llvm_ctx("classify ingress admission")?;
            builder
                .build_conditional_branch(accepted, done, rejected)
                .llvm_ctx("release only refused ingress owner")?;
            builder.position_at_end(rejected);
            let action = self
                .module
                .actor_recipes
                .get(ty)
                .and_then(|recipe| recipe.destroy)
                .ok_or_else(|| {
                    CodegenError::FailClosed("ingress payload lacks destructor".into())
                })?;
            ValueEmitter {
                module: self.module,
                ctx: self.ctx,
                llvm: &self.llvm,
                builder: &builder,
                value: function,
            }
            .destroy_loaded_value(value, layout, action)?;
            builder
                .build_unconditional_branch(done)
                .llvm_ctx("finish refused ingress cleanup")?;
            builder.position_at_end(done);
        }
        builder
            .build_return(Some(&status))
            .llvm_ctx("return ingress admission status")?;
        Ok(())
    }
}
