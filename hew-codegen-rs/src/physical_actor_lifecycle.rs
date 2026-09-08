//! Typed lifecycle callbacks at the native actor boundary.

use super::*;

impl<'ctx> ModuleEmitter<'ctx, '_> {
    fn lifecycle_word(
        &self,
        builder: &Builder<'ctx>,
        data: PointerValue<'ctx>,
        offset: usize,
        bits: u32,
    ) -> CodegenResult<IntValue<'ctx>> {
        // SAFETY: the caller validates the complete fixed-width runtime payload.
        let field = unsafe {
            builder.build_gep(
                self.ctx.i8_type(),
                data,
                &[self.ctx.i64_type().const_int(offset as u64, false)],
                "note.runtime.field",
            )
        }
        .llvm_ctx("address runtime notification field")?;
        Ok(builder
            .build_load(
                match bits {
                    32 => self.ctx.i32_type(),
                    64 => self.ctx.i64_type(),
                    _ => {
                        return Err(CodegenError::FailClosed(
                            "invalid notification word width".into(),
                        ))
                    }
                },
                field,
                "note.runtime.value",
            )
            .llvm_ctx("load runtime notification field")?
            .into_int_value())
    }

    fn lifecycle_variant_layout(
        &self,
        builtin: hew_types::BuiltinType,
    ) -> CodegenResult<&hew_mir::physical::PhysicalVariantLayout> {
        let name = match builtin {
            hew_types::BuiltinType::CrashKind => "std.failure.CrashKind",
            hew_types::BuiltinType::DownTarget => "std.link_monitor.DownTarget",
            hew_types::BuiltinType::DownReason => "std.link_monitor.DownReason",
            _ => return Err(CodegenError::FailClosed("unexpected lifecycle enum".into())),
        };
        self.module
            .target
            .variant_layout(&ResolvedTy::named_builtin(name, builtin, Vec::new()))
            .filter(|layout| !layout.is_indirect)
            .ok_or_else(|| {
                CodegenError::FailClosed(format!("missing inline notification layout for {name}"))
            })
    }

    fn lifecycle_variant_header(
        &self,
        builder: &Builder<'ctx>,
        destination: PointerValue<'ctx>,
        builtin: hew_types::BuiltinType,
        tag: IntValue<'ctx>,
    ) -> CodegenResult<()> {
        let layout = self.lifecycle_variant_layout(builtin)?;
        let function = builder
            .get_insert_block()
            .and_then(|block| block.get_parent())
            .ok_or_else(|| {
                CodegenError::FailClosed("notification decoder lacks function".into())
            })?;
        let valid = builder
            .build_int_compare(
                IntPredicate::ULT,
                tag,
                tag.get_type()
                    .const_int(layout.variants.len() as u64, false),
                "note.tag.valid",
            )
            .llvm_ctx("validate notification enum tag")?;
        let accepted = self.ctx.append_basic_block(function, "note.tag.accepted");
        let invalid = self.ctx.append_basic_block(function, "note.tag.invalid");
        builder
            .build_conditional_branch(valid, accepted, invalid)
            .llvm_ctx("guard notification enum tag")?;
        builder.position_at_end(invalid);
        builder
            .build_return(None)
            .llvm_ctx("ignore invalid notification enum")?;
        builder.position_at_end(accepted);
        let object = llvm_type(self.ctx, &layout.object.repr)?.into_struct_type();
        let tag_ty = object
            .get_field_type_at_index(0)
            .ok_or_else(|| CodegenError::FailClosed("notification enum lacks tag".into()))?
            .into_int_type();
        let tag = builder
            .build_int_cast(tag, tag_ty, "note.tag")
            .llvm_ctx("convert notification tag")?;
        let value = builder
            .build_insert_value(object.const_zero(), tag, 0, "note.enum")
            .llvm_ctx("construct notification enum")?;
        builder
            .build_store(destination, value)
            .llvm_ctx("store notification enum")?;
        Ok(())
    }

    fn lifecycle_variant_payload(
        &self,
        builder: &Builder<'ctx>,
        destination: PointerValue<'ctx>,
        builtin: hew_types::BuiltinType,
        variant: usize,
        fields: &[BasicValueEnum<'ctx>],
    ) -> CodegenResult<()> {
        let layout = self.lifecycle_variant_layout(builtin)?;
        let object = llvm_type(self.ctx, &layout.object.repr)?.into_struct_type();
        let payload_layout = layout
            .variants
            .get(variant)
            .ok_or_else(|| CodegenError::FailClosed("notification variant absent".into()))?;
        let payload_ty = llvm_type(self.ctx, &payload_layout.repr)?.into_struct_type();
        if payload_ty.count_fields() as usize != fields.len() {
            return Err(CodegenError::FailClosed(
                "notification variant field count differs".into(),
            ));
        }
        let mut value = payload_ty.const_zero();
        for (index, field) in fields.iter().enumerate() {
            value = builder
                .build_insert_value(value, *field, index as u32, "note.payload.field")
                .llvm_ctx("construct notification payload")?
                .into_struct_value();
        }
        let address = builder
            .build_struct_gep(object, destination, 1, "note.payload")
            .llvm_ctx("address notification enum payload")?;
        builder
            .build_store(address, value)
            .llvm_ctx("store notification enum payload")?;
        Ok(())
    }

    fn emit_down_payload(
        &self,
        builder: &Builder<'ctx>,
        function: FunctionValue<'ctx>,
        data: PointerValue<'ctx>,
        destination: PointerValue<'ctx>,
        object: inkwell::types::StructType<'ctx>,
    ) -> CodegenResult<()> {
        use hew_runtime::monitor::HewDownMessage;
        use hew_types::BuiltinType;
        let word = |offset, bits| self.lifecycle_word(builder, data, offset, bits);
        let monitor_id = word(std::mem::offset_of!(HewDownMessage, monitor_id), 64)?;
        let monitor = builder
            .build_struct_gep(object, destination, 0, "down.monitor")
            .llvm_ctx("address monitor identity")?;
        let monitor_ty = object
            .get_field_type_at_index(0)
            .ok_or_else(|| CodegenError::FailClosed("DOWN lacks monitor field".into()))?
            .into_struct_type();
        let monitor_value = builder
            .build_insert_value(monitor_ty.const_zero(), monitor_id, 0, "down.monitor.value")
            .llvm_ctx("construct monitor identity")?;
        builder
            .build_store(monitor, monitor_value)
            .llvm_ctx("store monitor identity")?;
        let target = builder
            .build_struct_gep(object, destination, 1, "down.target")
            .llvm_ctx("address DOWN target")?;
        let target_tag = word(std::mem::offset_of!(HewDownMessage, target_kind), 32)?;
        self.lifecycle_variant_header(builder, target, BuiltinType::DownTarget, target_tag)?;
        let local = self.ctx.append_basic_block(function, "down.local");
        let remote = self.ctx.append_basic_block(function, "down.remote");
        let reason = self.ctx.append_basic_block(function, "down.reason");
        let invalid = self.ctx.append_basic_block(function, "down.invalid");
        builder
            .build_switch(
                target_tag,
                invalid,
                &[
                    (self.ctx.i32_type().const_zero(), local),
                    (self.ctx.i32_type().const_int(1, false), remote),
                ],
            )
            .llvm_ctx("decode DOWN target")?;
        builder.position_at_end(invalid);
        builder
            .build_return(None)
            .llvm_ctx("ignore invalid DOWN tag")?;
        builder.position_at_end(local);
        let slot = word(std::mem::offset_of!(HewDownMessage, slot), 64)?;
        self.lifecycle_variant_payload(
            builder,
            target,
            BuiltinType::DownTarget,
            0,
            &[slot.into()],
        )?;
        builder
            .build_unconditional_branch(reason)
            .llvm_ctx("finish local DOWN target")?;
        builder.position_at_end(remote);
        let remote_layout = self.lifecycle_variant_layout(BuiltinType::DownTarget)?;
        let fields = llvm_type(self.ctx, &remote_layout.variants[1].repr)?.into_struct_type();
        let location_ty = fields
            .get_field_type_at_index(0)
            .ok_or_else(|| CodegenError::FailClosed("remote DOWN lacks location".into()))?
            .into_struct_type();
        let mut location = location_ty.const_zero();
        for (index, (offset, bits)) in [
            (std::mem::offset_of!(HewDownMessage, node_hi), 64),
            (std::mem::offset_of!(HewDownMessage, node_lo), 64),
            (std::mem::offset_of!(HewDownMessage, slot), 64),
            (
                std::mem::offset_of!(HewDownMessage, session_incarnation),
                32,
            ),
        ]
        .into_iter()
        .enumerate()
        {
            location = builder
                .build_insert_value(
                    location,
                    word(offset, bits)?,
                    index as u32,
                    "down.location.field",
                )
                .llvm_ctx("construct remote DOWN location")?
                .into_struct_value();
        }
        self.lifecycle_variant_payload(
            builder,
            target,
            BuiltinType::DownTarget,
            1,
            &[location.into()],
        )?;
        builder
            .build_unconditional_branch(reason)
            .llvm_ctx("finish remote DOWN target")?;
        builder.position_at_end(reason);
        let reason_ptr = builder
            .build_struct_gep(object, destination, 2, "down.reason")
            .llvm_ctx("address DOWN reason")?;
        let reason_tag = word(std::mem::offset_of!(HewDownMessage, reason_kind), 32)?;
        self.lifecycle_variant_header(builder, reason_ptr, BuiltinType::DownReason, reason_tag)?;
        let crashed = self.ctx.append_basic_block(function, "down.crashed");
        let done = self.ctx.append_basic_block(function, "down.decoded");
        builder
            .build_switch(
                reason_tag,
                invalid,
                &[
                    (self.ctx.i32_type().const_zero(), done),
                    (self.ctx.i32_type().const_int(1, false), crashed),
                    (self.ctx.i32_type().const_int(2, false), done),
                    (self.ctx.i32_type().const_int(3, false), done),
                ],
            )
            .llvm_ctx("decode DOWN reason")?;
        builder.position_at_end(crashed);
        let kind_layout = self.lifecycle_variant_layout(BuiltinType::CrashKind)?;
        let kind_ty = llvm_type(self.ctx, &kind_layout.object.repr)?;
        let kind_ptr = builder
            .build_alloca(kind_ty, "down.crash.kind")
            .llvm_ctx("allocate DOWN crash kind")?;
        self.lifecycle_variant_header(
            builder,
            kind_ptr,
            BuiltinType::CrashKind,
            word(std::mem::offset_of!(HewDownMessage, crash_kind), 32)?,
        )?;
        let kind = builder
            .build_load(kind_ty, kind_ptr, "down.crash.value")
            .llvm_ctx("read DOWN crash kind")?;
        self.lifecycle_variant_payload(builder, reason_ptr, BuiltinType::DownReason, 1, &[kind])?;
        builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish crashed DOWN reason")?;
        builder.position_at_end(done);
        Ok(())
    }
    /// Adapt the supervisor's crash ABI to the typed `CrashInfo` / `CrashAction`
    /// hook body. The diagnostic string is retained into the owned record and
    /// released by the hook's normal aggregate drop path.
    pub(super) fn emit_actor_crash(&self, actor: &SemActor) -> CodegenResult<()> {
        let hook = actor
            .crash
            .ok_or_else(|| CodegenError::FailClosed("crash descriptor lacks hook".into()))?;
        let callable = callable(self.module, hook)?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let info = self.ctx.i64_type();
        let action_ty = callable
            .return_layout
            .as_ref()
            .ok_or_else(|| CodegenError::FailClosed("crash hook lacks action layout".into()))?;
        let action_llvm = llvm_type(self.ctx, &action_ty.repr)?;
        let function = self.llvm.add_function(
            &symbol(actor.id, "on_crash"),
            self.ctx
                .i32_type()
                .fn_type(&[ptr.into(), info.into(), ptr.into()], false),
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(function, "entry"));
        let state = function.get_nth_param(0).unwrap().into_pointer_value();
        let code = function.get_nth_param(1).unwrap();
        let message = function.get_nth_param(2).unwrap().into_pointer_value();
        let crash_layout = &callable
            .params
            .get(1)
            .ok_or_else(|| CodegenError::FailClosed("crash hook lacks info parameter".into()))?
            .layout;
        let crash_object = builder
            .build_alloca(llvm_type(self.ctx, &crash_layout.repr)?, "crash.info")
            .llvm_ctx("allocate crash info")?;
        let crash_struct = llvm_type(self.ctx, &crash_layout.repr)?.into_struct_type();
        let code_ptr = builder
            .build_struct_gep(crash_struct, crash_object, 0, "crash.code")
            .llvm_ctx("address crash code")?;
        builder
            .build_store(code_ptr, code)
            .llvm_ctx("store crash code")?;
        let msg_ptr = builder
            .build_struct_gep(crash_struct, crash_object, 1, "crash.message")
            .llvm_ctx("address crash message")?;
        let clone = get_or_declare_external(
            &self.llvm,
            "hew_string_clone",
            ptr.fn_type(&[ptr.into()], false),
        )?;
        let cloned = builder
            .build_call(clone, &[message.into()], "crash.message.clone")
            .llvm_ctx("retain crash message")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("crash message clone returned void".into()))?;
        builder
            .build_store(msg_ptr, cloned)
            .llvm_ctx("store crash message")?;
        let fault = builder
            .build_alloca(ptr, "crash.fault")
            .llvm_ctx("allocate crash hook fault")?;
        builder
            .build_store(fault, ptr.const_null())
            .llvm_ctx("initialize crash hook fault")?;
        let output = builder
            .build_alloca(action_llvm, "crash.action")
            .llvm_ctx("allocate crash action")?;
        let mut args: Vec<BasicMetadataValueEnum<'ctx>> = vec![state.into(), crash_object.into()];
        if callable.return_layout.is_some() {
            args.push(output.into());
        }
        args.push(fault.into());
        builder
            .build_call(self.functions[&hook], &args, "crash.hook")
            .llvm_ctx("invoke crash hook")?;
        let returned_fault = builder
            .build_load(ptr, fault, "crash.returned_fault")
            .llvm_ctx("read crash hook fault")?
            .into_pointer_value();
        let failed = builder
            .build_is_not_null(returned_fault, "crash.failed")
            .llvm_ctx("test crash hook fault")?;
        let success = self.ctx.append_basic_block(function, "success");
        let failure = self.ctx.append_basic_block(function, "failure");
        builder
            .build_conditional_branch(failed, failure, success)
            .llvm_ctx("branch on crash hook completion")?;
        builder.position_at_end(failure);
        let drop_fault = get_or_declare_external(
            &self.llvm,
            "hew_fault_drop",
            self.ctx.void_type().fn_type(&[ptr.into()], false),
        )?;
        builder
            .build_call(drop_fault, &[returned_fault.into()], "")
            .llvm_ctx("release failed crash hook")?;
        builder
            .build_return(Some(&self.ctx.i32_type().const_int(1, false)))
            .llvm_ctx("escalate a failed crash hook")?;
        builder.position_at_end(success);
        let result = builder
            .build_load(action_llvm, output, "crash.action.value")
            .llvm_ctx("read crash action")?
            .into_struct_value();
        let tag = builder
            .build_extract_value(result, 0, "crash.action.tag")
            .llvm_ctx("read crash action tag")?
            .into_int_value();
        let action = builder
            .build_int_cast(tag, self.ctx.i32_type(), "crash.action.abi")
            .llvm_ctx("convert crash action tag")?;
        builder
            .build_return(Some(&action))
            .llvm_ctx("return crash action")?;
        Ok(())
    }

    /// Route runtime EXIT/DOWN signals to the actor's typed lifecycle hook.
    /// The system lane is separate from application dispatch, so these hooks
    /// cannot be forged by an ordinary actor message.
    pub(super) fn emit_actor_sys_dispatch(&self, actor: &SemActor) -> CodegenResult<()> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let target = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let function = self.llvm.add_function(
            &symbol(actor.id, "sys_dispatch"),
            self.ctx.void_type().fn_type(
                &[
                    ptr.into(),
                    ptr.into(),
                    self.ctx.i32_type().into(),
                    ptr.into(),
                    size_ty.into(),
                ],
                false,
            ),
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(function, "entry"));
        let message = function.get_nth_param(2).unwrap().into_int_value();
        let data = function.get_nth_param(3).unwrap().into_pointer_value();
        let data_size = function.get_nth_param(4).unwrap().into_int_value();
        let done = self.ctx.append_basic_block(function, "done");
        let exit = self.ctx.append_basic_block(function, "exit");
        let down = self.ctx.append_basic_block(function, "down");
        let unknown = self.ctx.append_basic_block(function, "unknown");
        builder
            .build_switch(
                message,
                unknown,
                &[
                    (
                        self.ctx.i32_type().const_int(
                            hew_runtime::mailbox_header::HewSysMsg::Exit.as_i32() as u64,
                            false,
                        ),
                        exit,
                    ),
                    (
                        self.ctx.i32_type().const_int(
                            hew_runtime::mailbox_header::HewSysMsg::Down.as_i32() as u64,
                            false,
                        ),
                        down,
                    ),
                ],
            )
            .llvm_ctx("dispatch actor lifecycle signal")?;
        builder.position_at_end(unknown);
        builder
            .build_unconditional_branch(done)
            .llvm_ctx("ignore unknown lifecycle signal")?;
        if let Some(hook) = actor.exit {
            builder.position_at_end(exit);
            self.emit_actor_notification_call(&builder, function, data, data_size, hook, true)?;
            builder
                .build_unconditional_branch(done)
                .llvm_ctx("finish exit hook")?;
        } else {
            builder.position_at_end(exit);
            builder
                .build_unconditional_branch(done)
                .llvm_ctx("ignore unhandled exit")?;
        }
        if let Some(hook) = actor.down {
            builder.position_at_end(down);
            self.emit_actor_notification_call(&builder, function, data, data_size, hook, false)?;
            builder
                .build_unconditional_branch(done)
                .llvm_ctx("finish down hook")?;
        } else {
            builder.position_at_end(down);
            builder
                .build_unconditional_branch(done)
                .llvm_ctx("ignore unhandled down")?;
        }
        builder.position_at_end(done);
        builder
            .build_return(None)
            .llvm_ctx("return from actor lifecycle signal")?;
        Ok(())
    }

    fn emit_actor_notification_call(
        &self,
        builder: &Builder<'ctx>,
        function: FunctionValue<'ctx>,
        data: PointerValue<'ctx>,
        data_size: IntValue<'ctx>,
        hook: hew_mir::physical::CallableId,
        exit: bool,
    ) -> CodegenResult<()> {
        let ctx = function.get_nth_param(0).unwrap().into_pointer_value();
        let state = function.get_nth_param(1).unwrap().into_pointer_value();
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let callable = callable(self.module, hook)?;
        let parameter = callable.params.get(1).ok_or_else(|| {
            CodegenError::FailClosed("lifecycle hook lacks its payload parameter".into())
        })?;
        let required = if exit {
            std::mem::size_of::<hew_runtime::link::ExitMessage>()
        } else {
            std::mem::size_of::<hew_runtime::monitor::HewDownMessage>()
        };
        let present = builder
            .build_is_not_null(data, "note.present")
            .llvm_ctx("check notification pointer")?;
        let sized = builder
            .build_int_compare(
                IntPredicate::EQ,
                data_size,
                data_size.get_type().const_int(required as u64, false),
                "note.sized",
            )
            .llvm_ctx("check notification size")?;
        let valid = builder
            .build_and(present, sized, "note.valid")
            .llvm_ctx("validate notification buffer")?;
        let accepted = self.ctx.append_basic_block(function, "note.accepted");
        let invalid = self.ctx.append_basic_block(function, "note.invalid");
        builder
            .build_conditional_branch(valid, accepted, invalid)
            .llvm_ctx("guard notification loads")?;
        builder.position_at_end(invalid);
        builder
            .build_return(None)
            .llvm_ctx("ignore malformed notification")?;
        builder.position_at_end(accepted);
        let object_ty = llvm_type(self.ctx, &parameter.layout.repr)?.into_struct_type();
        let payload = builder
            .build_alloca(object_ty, "note.source")
            .llvm_ctx("allocate source notification")?;
        builder
            .build_store(payload, object_ty.const_zero())
            .llvm_ctx("initialize source notification")?;
        if exit {
            let id = self.lifecycle_word(
                builder,
                data,
                std::mem::offset_of!(hew_runtime::link::ExitMessage, crashed_actor_id),
                64,
            )?;
            let field = builder
                .build_struct_gep(object_ty, payload, 0, "note.actor")
                .llvm_ctx("address notification actor")?;
            builder
                .build_store(field, id)
                .llvm_ctx("store notification actor")?;
            let kind = self.lifecycle_word(
                builder,
                data,
                std::mem::offset_of!(hew_runtime::link::ExitMessage, crash_kind),
                32,
            )?;
            let field = builder
                .build_struct_gep(object_ty, payload, 1, "note.kind")
                .llvm_ctx("address notification kind")?;
            self.lifecycle_variant_header(builder, field, hew_types::BuiltinType::CrashKind, kind)?;
        } else {
            self.emit_down_payload(builder, function, data, payload, object_ty)?;
        }
        let fault = builder
            .build_alloca(self.ctx.ptr_type(AddressSpace::default()), "hook.fault")
            .llvm_ctx("allocate lifecycle hook fault")?;
        builder
            .build_store(
                fault,
                self.ctx.ptr_type(AddressSpace::default()).const_null(),
            )
            .llvm_ctx("initialize lifecycle hook fault")?;
        let mut args: Vec<BasicMetadataValueEnum<'ctx>> = vec![state.into()];
        let parameter = callable.params.get(1).ok_or_else(|| {
            CodegenError::FailClosed("lifecycle hook lacks its payload parameter".into())
        })?;
        args.push(match parameter.carrier {
            ParamCarrier::Indirect => payload.into(),
            ParamCarrier::Direct => builder
                .build_load(
                    llvm_type(self.ctx, &parameter.layout.repr)?,
                    payload,
                    "hook.payload",
                )
                .llvm_ctx("load lifecycle hook payload")?
                .into(),
        });
        if let Some(layout) = &callable.return_layout {
            let output = builder
                .build_alloca(llvm_type(self.ctx, &layout.repr)?, "hook.output")
                .llvm_ctx("allocate lifecycle hook output")?;
            args.push(output.into());
        }
        args.push(fault.into());
        builder
            .build_call(self.functions[&hook], &args, "hook.status")
            .llvm_ctx("invoke lifecycle hook")?;
        let fault_value = builder
            .build_load(
                self.ctx.ptr_type(AddressSpace::default()),
                fault,
                "hook.fault.value",
            )
            .llvm_ctx("read lifecycle hook fault")?;
        let publish = get_or_declare_external(
            &self.llvm,
            "hew_actor_dispatch_set_fault",
            self.ctx
                .void_type()
                .fn_type(&[ptr.into(), ptr.into()], false),
        )?;
        builder
            .build_call(
                publish,
                &[ctx.into(), fault_value.into()],
                "hook.publish_fault",
            )
            .llvm_ctx("publish lifecycle hook fault")?;
        Ok(())
    }
}
