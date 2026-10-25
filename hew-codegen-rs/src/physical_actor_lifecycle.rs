//! Typed lifecycle callbacks at the native actor boundary.

use super::*;

fn native_crash_context_type(ctx: &Context) -> StructType<'_> {
    let ptr = ctx.ptr_type(AddressSpace::default());
    ctx.struct_type(
        &[
            ptr.into(),
            ctx.i32_type().into(),
            ptr.into(),
            ctx.i64_type().into(),
            ctx.i32_type().into(),
        ],
        false,
    )
}

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
            hew_types::BuiltinType::DownTarget => "std.failure.DownTarget",
            hew_types::BuiltinType::DownReason => "std.link_monitor.DownReason",
            _ => return Err(CodegenError::FailClosed("unexpected lifecycle enum".into())),
        };
        self.module
            .target
            .variant_layout(&ResolvedTy::named_builtin(builtin, Vec::new()))
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
            .build_return(Some(
                &self.ctx.ptr_type(AddressSpace::default()).const_null(),
            ))
            .llvm_ctx("ignore invalid notification enum")?;
        builder.position_at_end(accepted);
        self.lifecycle_variant_store(builder, destination, builtin, tag)
    }

    fn lifecycle_variant_store(
        &self,
        builder: &Builder<'ctx>,
        destination: PointerValue<'ctx>,
        builtin: hew_types::BuiltinType,
        tag: IntValue<'ctx>,
    ) -> CodegenResult<()> {
        let layout = self.lifecycle_variant_layout(builtin)?;
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

    fn emit_notification_target(
        &self,
        builder: &Builder<'ctx>,
        function: FunctionValue<'ctx>,
        data: PointerValue<'ctx>,
        target: PointerValue<'ctx>,
        base: usize,
    ) -> CodegenResult<()> {
        use hew_runtime::monitor::HewNotificationTarget;
        use hew_types::BuiltinType;
        let word = |offset, bits| self.lifecycle_word(builder, data, base + offset, bits);
        let target_tag = word(std::mem::offset_of!(HewNotificationTarget, kind), 32)?;
        let remote_role = builder
            .build_int_compare(
                inkwell::IntPredicate::EQ,
                target_tag,
                self.ctx.i32_type().const_int(
                    u64::from(hew_runtime::monitor::DOWN_TARGET_REMOTE_ROLE),
                    false,
                ),
                "down.remote.role",
            )
            .llvm_ctx("identify remote role DOWN")?;
        let source_tag = builder
            .build_select(
                remote_role,
                self.ctx.i32_type().const_int(1, false),
                target_tag,
                "down.source.tag",
            )
            .llvm_ctx("map remote role DOWN to Remote")?
            .into_int_value();
        self.lifecycle_variant_header(builder, target, BuiltinType::DownTarget, source_tag)?;
        let local = self.ctx.append_basic_block(function, "down.local");
        let remote = self.ctx.append_basic_block(function, "down.remote");
        let role = self.ctx.append_basic_block(function, "down.role");
        let reason = self
            .ctx
            .append_basic_block(function, "notification.target.done");
        let invalid = self.ctx.append_basic_block(function, "down.invalid");
        builder
            .build_switch(
                target_tag,
                invalid,
                &[
                    (self.ctx.i32_type().const_zero(), local),
                    (self.ctx.i32_type().const_int(1, false), remote),
                    (self.ctx.i32_type().const_int(2, false), role),
                    (
                        self.ctx.i32_type().const_int(
                            u64::from(hew_runtime::monitor::DOWN_TARGET_REMOTE_ROLE),
                            false,
                        ),
                        remote,
                    ),
                ],
            )
            .llvm_ctx("decode DOWN target")?;
        builder.position_at_end(invalid);
        builder
            .build_return(Some(
                &self.ctx.ptr_type(AddressSpace::default()).const_null(),
            ))
            .llvm_ctx("ignore invalid DOWN tag")?;
        builder.position_at_end(local);
        let slot = word(std::mem::offset_of!(HewNotificationTarget, slot), 64)?;
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
            (std::mem::offset_of!(HewNotificationTarget, node_hi), 64),
            (std::mem::offset_of!(HewNotificationTarget, node_lo), 64),
            (std::mem::offset_of!(HewNotificationTarget, slot), 64),
            (
                std::mem::offset_of!(HewNotificationTarget, session_incarnation),
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
        let target_kind = builder
            .build_int_z_extend(remote_role, self.ctx.i32_type(), "down.location.kind")
            .llvm_ctx("construct remote DOWN target kind")?;
        location = builder
            .build_insert_value(location, target_kind, 4, "down.location.target.kind")
            .llvm_ctx("insert remote DOWN target kind")?
            .into_struct_value();
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
        builder.position_at_end(role);
        let role_token = word(std::mem::offset_of!(HewNotificationTarget, slot), 64)?;
        self.lifecycle_variant_payload(
            builder,
            target,
            BuiltinType::DownTarget,
            2,
            &[role_token.into()],
        )?;
        builder
            .build_unconditional_branch(reason)
            .llvm_ctx("finish role DOWN target")?;
        builder.position_at_end(reason);
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
        self.emit_notification_target(
            builder,
            function,
            data,
            target,
            std::mem::offset_of!(HewDownMessage, target),
        )?;
        let invalid = self.ctx.append_basic_block(function, "down.invalid.reason");
        let reason_start = builder
            .get_insert_block()
            .ok_or_else(|| CodegenError::FailClosed("DOWN decoder lacks insertion block".into()))?;
        builder.position_at_end(invalid);
        builder
            .build_return(Some(
                &self.ctx.ptr_type(AddressSpace::default()).const_null(),
            ))
            .llvm_ctx("ignore invalid DOWN reason")?;
        builder.position_at_end(reason_start);
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
                    (self.ctx.i32_type().const_int(1, false), done),
                    (self.ctx.i32_type().const_int(2, false), crashed),
                    (self.ctx.i32_type().const_int(3, false), done),
                    (self.ctx.i32_type().const_int(4, false), done),
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
        self.lifecycle_variant_payload(builder, reason_ptr, BuiltinType::DownReason, 2, &[kind])?;
        builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish crashed DOWN reason")?;
        builder.position_at_end(done);
        Ok(())
    }
    /// Run the restart ruling on the existing cleanup continuation. Its own
    /// checked deadline is distinct from the later shielded state release.
    #[expect(
        clippy::too_many_lines,
        reason = "one retained context spans deadline entry, checked invocation and drained release"
    )]
    pub(super) fn emit_actor_crash(&self, actor: &SemActor) -> CodegenResult<()> {
        let hook = actor
            .crash
            .ok_or_else(|| CodegenError::FailClosed("crash descriptor lacks hook".into()))?;
        let callable = callable(self.module, hook)?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let i64_ty = self.ctx.i64_type();
        let i32_ty = self.ctx.i32_type();
        let context_ty = native_crash_context_type(self.ctx);
        release::custom(
            self.ctx,
            &self.llvm,
            self.module,
            &symbol(actor.id, "on_crash"),
            |values, frame, context| {
                let builder = values.builder;
                let (fault, status) = values.fault_sink.ok_or_else(|| {
                    CodegenError::FailClosed("crash hook lacks fault output".into())
                })?;
                let field = |index, name| {
                    builder
                        .build_struct_gep(context_ty, context, index, name)
                        .llvm_ctx("address retained crash context")
                };
                let token_fn = get_or_declare_external(
                    &self.llvm,
                    "hew_coro_state_token",
                    ptr.fn_type(&[ptr.into()], false),
                )?;
                let token = call_value(
                    builder,
                    token_fn,
                    &[frame.state.into()],
                    "crash.enclosing.token",
                )?;
                let scope_new = get_or_declare_external(
                    &self.llvm,
                    "hew_checked_scope_new",
                    ptr.fn_type(&[ptr.into()], false),
                )?;
                let scope = call_value(builder, scope_new, &[token.into()], "crash.scope")?;
                let duration = builder
                    .build_load(i64_ty, field(3, "crash.budget")?, "crash.budget.ns")
                    .llvm_ctx("read child stop budget")?;
                let deadline = get_or_declare_external(
                    &self.llvm,
                    "hew_checked_scope_deadline",
                    self.ctx
                        .void_type()
                        .fn_type(&[ptr.into(), i64_ty.into()], false),
                )?;
                builder
                    .build_call(deadline, &[scope.into(), duration.into()], "")
                    .llvm_ctx("arm crash hook stop budget")?;
                let scope_token = get_or_declare_external(
                    &self.llvm,
                    "hew_task_scope_cancel_token",
                    ptr.fn_type(&[ptr.into()], false),
                )?;
                let token = call_value(builder, scope_token, &[scope.into()], "crash.scope.token")?;
                let enter = get_or_declare_external(
                    &self.llvm,
                    "hew_coro_state_enter_token",
                    self.ctx.void_type().fn_type(&[ptr.into(); 2], false),
                )?;
                builder
                    .build_call(enter, &[frame.state.into(), token.into()], "")
                    .llvm_ctx("enter crash hook deadline")?;
                let cancelled = get_or_declare_external(
                    &self.llvm,
                    "hew_coro_state_is_cancelled",
                    i32_ty.fn_type(&[ptr.into()], false),
                )?;
                let cancelled =
                    call_value(builder, cancelled, &[frame.state.into()], "crash.expired")?
                        .into_int_value();
                let expired = builder
                    .build_int_compare(
                        IntPredicate::NE,
                        cancelled,
                        i32_ty.const_zero(),
                        "crash.skip",
                    )
                    .llvm_ctx("check initial crash hook budget")?;
                let run = self.ctx.append_basic_block(values.value, "crash.run");
                let skip = self.ctx.append_basic_block(values.value, "crash.expired");
                let done = self.ctx.append_basic_block(values.value, "crash.done");
                builder
                    .build_conditional_branch(expired, skip, run)
                    .llvm_ctx("guard crash hook entry")?;
                builder.position_at_end(skip);
                let code = i32_ty.const_int(hew_runtime::fault::HEW_FAULT_DEADLINE as u64, true);
                let new_fault = get_or_declare_external(
                    &self.llvm,
                    "hew_fault_new",
                    ptr.fn_type(&[i32_ty.into()], false),
                )?;
                let raised =
                    call_value(builder, new_fault, &[code.into()], "crash.deadline.fault")?;
                builder
                    .build_store(fault, raised)
                    .llvm_ctx("retain crash deadline fault")?;
                builder
                    .build_store(status, code)
                    .llvm_ctx("retain crash deadline status")?;
                builder
                    .build_unconditional_branch(done)
                    .llvm_ctx("skip expired crash hook")?;
                builder.position_at_end(run);
                let state = builder
                    .build_load(ptr, field(0, "crash.state")?, "crash.actor.state")
                    .llvm_ctx("borrow initialized crash state")?;
                let kind = builder
                    .build_load(i32_ty, field(1, "crash.kind")?, "crash.kind.value")
                    .llvm_ctx("read crash kind")?
                    .into_int_value();
                let message = builder
                    .build_load(ptr, field(2, "crash.message")?, "crash.message.value")
                    .llvm_ctx("borrow crash diagnostic")?;
                let layout = &callable
                    .params
                    .get(1)
                    .ok_or_else(|| {
                        CodegenError::FailClosed("crash hook lacks info parameter".into())
                    })?
                    .layout;
                let object_ty = llvm_type(self.ctx, &layout.repr)?.into_struct_type();
                let object = frame.storage(self.ctx, object_ty.into(), "crash.info")?;
                let kind_ptr = builder
                    .build_struct_gep(object_ty, object, 0, "crash.info.kind")
                    .llvm_ctx("address crash info kind")?;
                self.lifecycle_variant_store(
                    builder,
                    kind_ptr,
                    hew_types::BuiltinType::CrashKind,
                    kind,
                )?;
                let clone = get_or_declare_external(
                    &self.llvm,
                    "hew_string_clone",
                    ptr.fn_type(&[ptr.into()], false),
                )?;
                let owned = call_value(builder, clone, &[message.into()], "crash.message.clone")?;
                let message_ptr = builder
                    .build_struct_gep(object_ty, object, 1, "crash.info.message")
                    .llvm_ctx("address crash info message")?;
                builder
                    .build_store(message_ptr, owned)
                    .llvm_ctx("store owned crash info message")?;
                let output_ty = llvm_type(
                    self.ctx,
                    &callable
                        .return_layout
                        .as_ref()
                        .ok_or_else(|| {
                            CodegenError::FailClosed("crash hook lacks action layout".into())
                        })?
                        .repr,
                )?;
                let output = frame.storage(self.ctx, output_ty, "crash.action")?;
                let args = [state.into(), object.into(), output.into(), fault.into()];
                let outcome = if callable.is_resumable {
                    suspend::invoke_child(
                        self.ctx,
                        &self.llvm,
                        builder,
                        values.value,
                        frame,
                        self.ramps[&hook],
                        &args,
                    )?
                } else {
                    call_value(builder, self.functions[&hook], &args, "crash.hook.status")?
                        .into_int_value()
                };
                builder
                    .build_store(status, outcome)
                    .llvm_ctx("retain crash hook outcome")?;
                let success = self.ctx.append_basic_block(values.value, "crash.success");
                let ok = builder
                    .build_int_compare(IntPredicate::EQ, outcome, i32_ty.const_zero(), "crash.ok")
                    .llvm_ctx("check crash hook outcome")?;
                builder
                    .build_conditional_branch(ok, success, done)
                    .llvm_ctx("accept only a completed crash ruling")?;
                builder.position_at_end(success);
                let action = builder
                    .build_load(output_ty, output, "crash.action.value")
                    .llvm_ctx("read crash ruling")?
                    .into_struct_value();
                let tag = builder
                    .build_extract_value(action, 0, "crash.action.tag")
                    .llvm_ctx("read crash ruling tag")?
                    .into_int_value();
                let tag = builder
                    .build_int_cast(tag, i32_ty, "crash.action.abi")
                    .llvm_ctx("convert crash ruling tag")?;
                builder
                    .build_store(field(4, "crash.action.out")?, tag)
                    .llvm_ctx("publish crash ruling to its owner")?;
                builder
                    .build_unconditional_branch(done)
                    .llvm_ctx("finish crash ruling")?;
                builder.position_at_end(done);
                let leave = get_or_declare_external(
                    &self.llvm,
                    "hew_coro_state_leave_token",
                    self.ctx.void_type().fn_type(&[ptr.into()], false),
                )?;
                builder
                    .build_call(leave, &[frame.state.into()], "")
                    .llvm_ctx("leave drained crash hook deadline")?;
                let close = get_or_declare_external(
                    &self.llvm,
                    "hew_checked_scope_close",
                    self.ctx.void_type().fn_type(&[ptr.into()], false),
                )?;
                builder
                    .build_call(close, &[scope.into()], "")
                    .llvm_ctx("release drained crash hook deadline")?;
                Ok(())
            },
        )?;
        Ok(())
    }

    /// Route runtime EXIT/DOWN signals to the actor's typed lifecycle hook.
    /// The system lane is separate from application dispatch, so these hooks
    /// cannot be forged by an ordinary actor message. Like application
    /// dispatch, it returns the parked continuation of a hook that suspended,
    /// or null once the signal is handled.
    pub(super) fn emit_actor_sys_dispatch(&self, actor: &SemActor) -> CodegenResult<()> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let target = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let function = self.llvm.add_function(
            &symbol(actor.id, "sys_dispatch"),
            ptr.fn_type(
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
        builder.position_at_end(exit);
        if let Some(hook) = actor.exit {
            self.emit_actor_notification(&builder, function, actor, data, data_size, hook, true)?;
        } else {
            self.emit_unhandled_actor_exit(&builder, function, data, data_size, done)?;
        }
        builder.position_at_end(down);
        if let Some(hook) = actor.down {
            self.emit_actor_notification(&builder, function, actor, data, data_size, hook, false)?;
        } else {
            builder
                .build_unconditional_branch(done)
                .llvm_ctx("ignore unhandled down")?;
        }
        builder.position_at_end(done);
        builder
            .build_return(Some(&ptr.const_null()))
            .llvm_ctx("return from actor lifecycle signal")?;
        Ok(())
    }

    /// Publish the default linked failure through the checked activation.
    /// The callback has the C ABI, so it must never call an unwinding trap.
    fn emit_unhandled_actor_exit(
        &self,
        builder: &Builder<'ctx>,
        function: FunctionValue<'ctx>,
        data: PointerValue<'ctx>,
        data_size: IntValue<'ctx>,
        done: BasicBlock<'ctx>,
    ) -> CodegenResult<()> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let present = builder
            .build_is_not_null(data, "exit.present")
            .llvm_ctx("check EXIT payload pointer")?;
        let sized = builder
            .build_int_compare(
                IntPredicate::EQ,
                data_size,
                data_size.get_type().const_int(
                    std::mem::size_of::<hew_runtime::link::ExitMessage>() as u64,
                    false,
                ),
                "exit.sized",
            )
            .llvm_ctx("check EXIT payload size")?;
        let valid = builder
            .build_and(present, sized, "exit.valid")
            .llvm_ctx("validate default EXIT payload")?;
        let fail = self.ctx.append_basic_block(function, "exit.unhandled");
        builder
            .build_conditional_branch(valid, fail, done)
            .llvm_ctx("guard default EXIT decoding")?;
        builder.position_at_end(fail);
        let reason = self.lifecycle_word(
            builder,
            data,
            std::mem::offset_of!(hew_runtime::link::ExitMessage, reason),
            32,
        )?;
        let clean = builder
            .build_int_compare(
                IntPredicate::EQ,
                reason,
                self.ctx.i32_type().const_zero(),
                "exit.clean",
            )
            .llvm_ctx("classify linked terminal reason")?;
        let code = builder
            .build_select(
                clean,
                self.ctx.i32_type().const_int(
                    hew_runtime::internal::types::HewActorState::Crashed as u64,
                    false,
                ),
                reason,
                "exit.crash.code",
            )
            .llvm_ctx("make an unhandled clean EXIT a crash")?;
        let new_fault = get_or_declare_external(
            &self.llvm,
            "hew_fault_new",
            ptr.fn_type(&[self.ctx.i32_type().into()], false),
        )?;
        let fault = call_value(builder, new_fault, &[code.into()], "exit.fault")?;
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
                &[function.get_nth_param(0).unwrap().into(), fault.into()],
                "",
            )
            .llvm_ctx("transfer unhandled EXIT fault to checked dispatch")?;
        builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish unhandled EXIT")?;
        Ok(())
    }

    /// Validate a runtime notification, then run its hook: synchronously, or
    /// through a ramp whose continuation the system lane parks when it suspends.
    #[allow(clippy::too_many_arguments, reason = "one typed lifecycle signal")]
    fn emit_actor_notification(
        &self,
        builder: &Builder<'ctx>,
        function: FunctionValue<'ctx>,
        actor: &SemActor,
        data: PointerValue<'ctx>,
        data_size: IntValue<'ctx>,
        hook: hew_mir::physical::CallableId,
        exit: bool,
    ) -> CodegenResult<()> {
        let ctx = function.get_nth_param(0).unwrap().into_pointer_value();
        let state = function.get_nth_param(1).unwrap().into_pointer_value();
        let ptr = self.ctx.ptr_type(AddressSpace::default());
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
            .build_return(Some(&ptr.const_null()))
            .llvm_ctx("ignore malformed notification")?;
        builder.position_at_end(accepted);
        let payload = self.decode_notification(builder, function, data, hook, exit)?;
        if callable(self.module, hook)?.is_resumable {
            let ramp = self.emit_actor_notification_ramp(actor, hook, exit)?;
            let handle = call_value(builder, ramp, &[state.into(), payload.into()], "hook.frame")?
                .into_pointer_value();
            let is_done = get_or_declare_external(
                &self.llvm,
                "hew_cont_done",
                self.ctx.bool_type().fn_type(&[ptr.into()], false),
            )?;
            let complete =
                call_value(builder, is_done, &[handle.into()], "hook.done")?.into_int_value();
            let ready = self.ctx.append_basic_block(function, "hook.ready");
            let pending = self.ctx.append_basic_block(function, "hook.pending");
            builder
                .build_conditional_branch(complete, ready, pending)
                .llvm_ctx("select lifecycle hook completion")?;
            builder.position_at_end(pending);
            builder
                .build_return(Some(&handle))
                .llvm_ctx("park suspended lifecycle hook")?;
            builder.position_at_end(ready);
            let destroy = external_drop(self.ctx, &self.llvm, "hew_cont_destroy")?;
            builder
                .build_call(destroy, &[handle.into()], "")
                .llvm_ctx("destroy completed lifecycle hook")?;
            builder
                .build_return(Some(&ptr.const_null()))
                .llvm_ctx("finish completed lifecycle hook")?;
            return Ok(());
        }
        let fault = builder
            .build_alloca(ptr, "hook.fault")
            .llvm_ctx("allocate lifecycle hook fault")?;
        builder
            .build_store(fault, ptr.const_null())
            .llvm_ctx("initialize lifecycle hook fault")?;
        let args = self.notification_args(builder, state, payload, hook, fault)?;
        builder
            .build_call(self.functions[&hook], &args, "hook.status")
            .llvm_ctx("invoke lifecycle hook")?;
        let fault_value = builder
            .build_load(ptr, fault, "hook.fault.value")
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
        builder
            .build_return(Some(&ptr.const_null()))
            .llvm_ctx("finish lifecycle hook")?;
        Ok(())
    }

    /// A suspending EXIT/DOWN hook's continuation, shaped like a suspending
    /// handler's: it copies the decoded notification into its own frame before
    /// its first suspension, runs the hook and publishes its fault under the
    /// current activation.
    fn emit_actor_notification_ramp(
        &self,
        actor: &SemActor,
        hook: hew_mir::physical::CallableId,
        exit: bool,
    ) -> CodegenResult<FunctionValue<'ctx>> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let ramp = self.llvm.add_function(
            &symbol(actor.id, if exit { "exit_start" } else { "down_start" }),
            ptr.fn_type(&[ptr.into(), ptr.into()], false),
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(ramp, "entry"));
        let new_state = get_or_declare_external(
            &self.llvm,
            "hew_actor_coro_state_new",
            ptr.fn_type(&[], false),
        )?;
        let child = call_value(&builder, new_state, &[], "hook.invocation")?.into_pointer_value();
        let frame = coro::begin(self.ctx, &self.llvm, &builder, ramp, child)?;
        let fault = builder
            .build_alloca(ptr, "hook.fault")
            .llvm_ctx("allocate persistent lifecycle hook fault")?;
        builder
            .build_store(fault, ptr.const_null())
            .llvm_ctx("initialize lifecycle hook fault")?;
        let object_ty = self.notification_type(hook)?;
        let payload = builder
            .build_alloca(object_ty, "hook.notification")
            .llvm_ctx("allocate persistent notification")?;
        let source = builder
            .build_load(
                object_ty,
                ramp.get_nth_param(1).unwrap().into_pointer_value(),
                "hook.notification.value",
            )
            .llvm_ctx("read decoded notification")?;
        builder
            .build_store(payload, source)
            .llvm_ctx("keep the notification across suspension")?;
        let state = ramp.get_nth_param(0).unwrap().into_pointer_value();
        let args = self.notification_args(&builder, state, payload, hook, fault)?;
        suspend::invoke_child(
            self.ctx,
            &self.llvm,
            &builder,
            ramp,
            &frame,
            self.ramps[&hook],
            &args,
        )?;
        let completion = self.ctx.create_builder();
        completion.position_before(
            &frame.finish.get_terminator().ok_or_else(|| {
                CodegenError::FailClosed("lifecycle finish lacks its branch".into())
            })?,
        );
        let returned_fault = completion
            .build_load(ptr, fault, "hook.returned.fault")
            .llvm_ctx("read completed lifecycle hook fault")?;
        let finish = get_or_declare_external(
            &self.llvm,
            "hew_actor_coro_finish",
            self.ctx
                .void_type()
                .fn_type(&[ptr.into(), ptr.into()], false),
        )?;
        completion
            .build_call(finish, &[child.into(), returned_fault.into()], "")
            .llvm_ctx("finish lifecycle hook after retiring its managed view")?;
        builder
            .build_unconditional_branch(frame.finish)
            .llvm_ctx("finish lifecycle hook frame")?;
        Ok(ramp)
    }

    fn notification_type(
        &self,
        hook: hew_mir::physical::CallableId,
    ) -> CodegenResult<inkwell::types::StructType<'ctx>> {
        let parameter = callable(self.module, hook)?.params.get(1).ok_or_else(|| {
            CodegenError::FailClosed("lifecycle hook lacks its payload parameter".into())
        })?;
        Ok(llvm_type(self.ctx, &parameter.layout.repr)?.into_struct_type())
    }

    /// Decode a validated runtime notification into the hook's typed payload.
    fn decode_notification(
        &self,
        builder: &Builder<'ctx>,
        function: FunctionValue<'ctx>,
        data: PointerValue<'ctx>,
        hook: hew_mir::physical::CallableId,
        exit: bool,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let object_ty = self.notification_type(hook)?;
        let payload = builder
            .build_alloca(object_ty, "note.source")
            .llvm_ctx("allocate source notification")?;
        builder
            .build_store(payload, object_ty.const_zero())
            .llvm_ctx("initialize source notification")?;
        if exit {
            let field = builder
                .build_struct_gep(object_ty, payload, 0, "note.target")
                .llvm_ctx("address notification target")?;
            self.emit_notification_target(
                builder,
                function,
                data,
                field,
                std::mem::offset_of!(hew_runtime::link::ExitMessage, target),
            )?;
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
        Ok(payload)
    }

    /// The hook's arguments: state, the decoded payload by its carrier, the
    /// optional output slot and the fault slot.
    fn notification_args(
        &self,
        builder: &Builder<'ctx>,
        state: PointerValue<'ctx>,
        payload: PointerValue<'ctx>,
        hook: hew_mir::physical::CallableId,
        fault: PointerValue<'ctx>,
    ) -> CodegenResult<Vec<BasicMetadataValueEnum<'ctx>>> {
        let callable = callable(self.module, hook)?;
        let parameter = callable.params.get(1).ok_or_else(|| {
            CodegenError::FailClosed("lifecycle hook lacks its payload parameter".into())
        })?;
        let mut args: Vec<BasicMetadataValueEnum<'ctx>> = vec![state.into()];
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
        Ok(args)
    }
}

#[cfg(test)]
mod abi_tests {
    use super::super::actor::{c_mirror_mismatch, field_size};
    use super::*;
    use hew_runtime::actor_native::HewNativeCrashContext;
    use std::mem::{align_of, offset_of, size_of};

    #[test]
    fn runtime_crash_kind_tags_match_std_enum() {
        use hew_runtime::internal::types::CrashKind;

        let source = hew_types::builtin_enums::monomorphic_builtin_enum("std.failure.CrashKind")
            .expect("CrashKind must be derived from its stdlib declaration");
        // Runtime Crashed is exposed to Hew as Fault. Compare actual runtime
        // tags with the source-derived order rather than another list of tags.
        let kinds = [
            (CrashKind::Crashed, "Fault"),
            (CrashKind::HeapExceeded, "HeapExceeded"),
            (CrashKind::PartitionDetected, "PartitionDetected"),
            (CrashKind::HandlerFailed, "HandlerFailed"),
            (CrashKind::TimerOverdue, "TimerOverdue"),
        ];
        assert_eq!(source.variants.len(), kinds.len());
        for (runtime, name) in kinds {
            let source_tag = source
                .variants
                .iter()
                .position(|variant| variant.name == name)
                .unwrap_or_else(|| panic!("std.failure.CrashKind must declare {name}"));
            assert_eq!(
                usize::try_from(runtime.tag()).expect("crash tags are nonnegative"),
                source_tag,
                "runtime crash kind {runtime:?} must carry the std enum tag for {name}"
            );
        }
    }

    #[test]
    fn native_crash_context_matches_the_runtime_c_abi() {
        let ctx = Context::create();
        let fields = [
            (
                offset_of!(HewNativeCrashContext, state),
                field_size(|c: &HewNativeCrashContext| &c.state),
            ),
            (
                offset_of!(HewNativeCrashContext, kind),
                field_size(|c: &HewNativeCrashContext| &c.kind),
            ),
            (
                offset_of!(HewNativeCrashContext, message),
                field_size(|c: &HewNativeCrashContext| &c.message),
            ),
            (
                offset_of!(HewNativeCrashContext, stop_ns),
                field_size(|c: &HewNativeCrashContext| &c.stop_ns),
            ),
            (
                offset_of!(HewNativeCrashContext, action),
                field_size(|c: &HewNativeCrashContext| &c.action),
            ),
        ];
        let mismatch = |mirror| {
            c_mirror_mismatch(
                mirror,
                &fields,
                size_of::<HewNativeCrashContext>(),
                align_of::<HewNativeCrashContext>(),
            )
        };
        assert_eq!(mismatch(native_crash_context_type(&ctx)), None);
        // A misplaced narrow action must be rejected.
        let ptr = ctx.ptr_type(AddressSpace::default());
        let reordered = ctx.struct_type(
            &[
                ptr.into(),
                ctx.i32_type().into(),
                ptr.into(),
                ctx.i64_type().into(),
                ctx.i64_type().into(),
            ],
            false,
        );
        assert!(mismatch(reordered).is_some());
    }
}
