//! Native actor codec adapters and target-walked module registration.

use super::*;
use hew_mir::physical::SemActorHandler;
use inkwell::types::StructType;
use inkwell::values::AsValueRef;

impl<'ctx> ModuleEmitter<'ctx, '_> {
    fn codec_runtime(
        &self,
        builder: &Builder<'ctx>,
        name: &str,
        result: Option<BasicTypeEnum<'ctx>>,
        args: &[BasicValueEnum<'ctx>],
    ) -> CodegenResult<Option<BasicValueEnum<'ctx>>> {
        let params = args
            .iter()
            .map(|value| value.get_type().into())
            .collect::<Vec<_>>();
        let ty = result.map_or_else(
            || self.ctx.void_type().fn_type(&params, false),
            |ty| ty.fn_type(&params, false),
        );
        let callee = get_or_declare_external(&self.llvm, name, ty)?;
        let args = args.iter().copied().map(Into::into).collect::<Vec<_>>();
        Ok(builder
            .build_call(
                callee,
                &args,
                if result.is_some() {
                    "codec.runtime"
                } else {
                    ""
                },
            )
            .llvm_ctx("call actor codec runtime")?
            .try_as_basic_value()
            .basic())
    }

    pub(super) fn emit_actor_codec_registration(&self) -> CodegenResult<()> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let mut registrations = Vec::new();
        for actor in &self.module.actors {
            for handler in &actor.handlers {
                let Some(codec) = &handler.codec else {
                    continue;
                };
                let dispatch = self
                    .llvm
                    .get_function(&format!("__hew_actor_{}_dispatch", actor.id.0))
                    .ok_or_else(|| CodegenError::FailClosed("actor codec lacks dispatch".into()))?;
                let payload = actor::message_type(self.module, self.ctx, handler)?;
                for reply in [false, true] {
                    if reply && codec.reply.is_none() {
                        continue;
                    }
                    let plans = if reply {
                        codec.reply.iter().cloned().collect()
                    } else {
                        codec.params.clone()
                    };
                    let label = format!(
                        "__hew_actor_{}_codec_{}_{}",
                        actor.id.0,
                        handler.message_id,
                        if reply { "reply" } else { "request" }
                    );
                    let encode = self
                        .emit_actor_codec_adapter(&label, handler, payload, &plans, reply, false)?;
                    let decode = self
                        .emit_actor_codec_adapter(&label, handler, payload, &plans, reply, true)?;
                    registrations.push((dispatch, handler.message_id, encode, decode, reply));
                }
            }
        }
        if registrations.is_empty() {
            return Ok(());
        }
        let init = self.llvm.add_function(
            "hew_module_init_actor_codecs",
            self.ctx.void_type().fn_type(&[], false),
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(init, "entry"));
        for (dispatch, message, encode, decode, reply) in registrations {
            self.codec_runtime(
                &builder,
                if reply {
                    "hew_xnode_register_reply_codec"
                } else {
                    "hew_xnode_register_codec"
                },
                None,
                &[
                    dispatch.as_global_value().as_pointer_value().into(),
                    self.ctx
                        .i32_type()
                        .const_int(u64::from(message), false)
                        .into(),
                    encode.as_global_value().as_pointer_value().into(),
                    decode.as_global_value().as_pointer_value().into(),
                ],
            )?;
        }
        builder
            .build_return(None)
            .llvm_ctx("finish actor codec registration")?;
        let initializer = ptr.const_array(&[init.as_global_value().as_pointer_value()]);
        let global = self
            .llvm
            .add_global(initializer.get_type(), None, "hew.actor.codec.init");
        global.set_initializer(&initializer);
        global.set_linkage(Linkage::Internal);
        let section = if self.module.target.triple.contains("apple") {
            "__DATA,__mod_init_func,mod_init_funcs"
        } else if self.module.target.triple.contains("windows") {
            ".CRT$XCU"
        } else {
            ".init_array"
        };
        let section = std::ffi::CString::new(section).expect("static section has no NUL");
        // Inkwell's set_section applies host-specific Mach-O rewriting. Section
        // spelling must follow this module's target when cross-compiling.
        // SAFETY: the global and NUL-terminated section name remain live for this call.
        unsafe {
            inkwell::llvm_sys::core::LLVMSetSection(global.as_value_ref(), section.as_ptr());
        }
        let used = ptr.const_array(&[global.as_pointer_value()]);
        let retained = self.llvm.add_global(used.get_type(), None, "llvm.used");
        retained.set_initializer(&used);
        retained.set_linkage(Linkage::Appending);
        let metadata = c"llvm.metadata";
        // SAFETY: both the LLVM global and static section string are live.
        unsafe {
            inkwell::llvm_sys::core::LLVMSetSection(retained.as_value_ref(), metadata.as_ptr());
        }
        Ok(())
    }

    #[expect(
        clippy::too_many_arguments,
        reason = "the adapter realizes one checked request or reply in both codec directions"
    )]
    #[expect(
        clippy::too_many_lines,
        reason = "codec publication and malformed-input rollback share one ownership flow"
    )]
    fn emit_actor_codec_adapter(
        &self,
        name: &str,
        handler: &SemActorHandler,
        payload: StructType<'ctx>,
        plans: &[std::sync::Arc<hew_mir::physical::SemWirePlan>],
        reply: bool,
        decode: bool,
    ) -> CodegenResult<FunctionValue<'ctx>> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let target = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let signature = if decode {
            ptr.fn_type(&[ptr.into(), size_ty.into(), ptr.into()], false)
        } else {
            ptr.fn_type(&[ptr.into(), ptr.into()], false)
        };
        let function = self.llvm.add_function(
            &format!("{name}_{}", if decode { "decode" } else { "encode" }),
            signature,
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(function, "entry"));
        let runtime = |name, result, args: &[BasicValueEnum<'ctx>]| {
            self.codec_runtime(&builder, name, result, args)
        };
        let input = function.get_nth_param(0).unwrap().into_pointer_value();
        let cursor = if decode {
            runtime(
                "hew_cbor_de_new",
                Some(ptr.into()),
                &[input.into(), function.get_nth_param(1).unwrap()],
            )?
            .unwrap()
            .into_pointer_value()
        } else {
            runtime("hew_cbor_ser_new", Some(ptr.into()), &[])?
                .unwrap()
                .into_pointer_value()
        };
        let byte_size = if reply {
            self.module
                .target
                .layout(&handler.return_ty)
                .ok_or_else(|| CodegenError::FailClosed("actor codec reply lacks layout".into()))?
                .size
        } else {
            target.get_store_size(&payload)
        };
        let output = if decode {
            actor::allocate(self.module, self.ctx, &self.llvm, &builder, byte_size)?
        } else {
            input
        };
        let rollback = self.ctx.append_basic_block(function, "rollback");
        let fault = builder
            .build_alloca(ptr, "codec.fault")
            .llvm_ctx("allocate codec fault")?;
        builder
            .build_store(fault, ptr.const_null())
            .llvm_ctx("initialize codec fault")?;
        let mut owners = Vec::new();
        for (index, plan) in plans.iter().enumerate() {
            let slot = if reply {
                output
            } else {
                builder
                    .build_struct_gep(
                        payload,
                        output,
                        u32::try_from(index + 1).map_err(|_| {
                            CodegenError::FailClosed("actor codec index exceeds u32".into())
                        })?,
                        "codec.field",
                    )
                    .llvm_ctx("address native message field")?
            };
            let live = builder
                .build_alloca(self.ctx.bool_type(), "codec.live")
                .llvm_ctx("allocate decode live bit")?;
            builder
                .build_store(live, self.ctx.bool_type().const_zero())
                .llvm_ctx("initialize decode live bit")?;
            owners.push((slot, live, plan));
        }
        if !reply {
            runtime(
                if decode {
                    "hew_cbor_de_enter_array"
                } else {
                    "hew_cbor_ser_begin_array"
                },
                None,
                &[cursor.into()],
            )?;
        }
        for (slot, live, plan) in &owners {
            let callback = wire::emit_callback(
                self.module,
                self.ctx,
                &self.llvm,
                plan,
                &self.module.actor_recipes,
                &self.value_callbacks,
                decode,
            )?;
            if decode {
                if callback.get_type().get_return_type() != Some(self.ctx.i32_type().into()) {
                    return Err(CodegenError::FailClosed(
                        "actor decoding requires a synchronous portable value codec".into(),
                    ));
                }
                if !reply {
                    let available = runtime(
                        "hew_cbor_de_array_next",
                        Some(self.ctx.i32_type().into()),
                        &[cursor.into()],
                    )?
                    .unwrap()
                    .into_int_value();
                    let next = self.ctx.append_basic_block(function, "decode.field");
                    let yes = builder
                        .build_int_compare(
                            IntPredicate::EQ,
                            available,
                            self.ctx.i32_type().const_int(1, false),
                            "codec.available",
                        )
                        .llvm_ctx("check request arity")?;
                    builder
                        .build_conditional_branch(yes, next, rollback)
                        .llvm_ctx("reject missing request field")?;
                    builder.position_at_end(next);
                }
                let status = builder
                    .build_call(
                        callback,
                        &[cursor.into(), (*slot).into(), fault.into()],
                        "codec.status",
                    )
                    .llvm_ctx("decode actor field")?
                    .try_as_basic_value()
                    .basic()
                    .unwrap()
                    .into_int_value();
                let next = self.ctx.append_basic_block(function, "decode.next");
                let ok = builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        status,
                        self.ctx.i32_type().const_zero(),
                        "codec.ok",
                    )
                    .llvm_ctx("check decoded field")?;
                builder
                    .build_conditional_branch(ok, next, rollback)
                    .llvm_ctx("retain only initialized field")?;
                builder.position_at_end(next);
                builder
                    .build_store(*live, self.ctx.bool_type().const_int(1, false))
                    .llvm_ctx("own decoded field")?;
            } else {
                builder
                    .build_call(callback, &[cursor.into(), (*slot).into()], "")
                    .llvm_ctx("encode actor field")?;
            }
        }
        if decode {
            if !reply {
                let extra = runtime(
                    "hew_cbor_de_array_next",
                    Some(self.ctx.i32_type().into()),
                    &[cursor.into()],
                )?
                .unwrap()
                .into_int_value();
                let exact = self.ctx.append_basic_block(function, "decode.exact");
                let ok = builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        extra,
                        self.ctx.i32_type().const_zero(),
                        "codec.arity",
                    )
                    .llvm_ctx("check exact request arity")?;
                builder
                    .build_conditional_branch(ok, exact, rollback)
                    .llvm_ctx("reject extra request fields")?;
                builder.position_at_end(exact);
                runtime("hew_cbor_de_exit_array", None, &[cursor.into()])?;
            }
            let status = runtime(
                "hew_cbor_de_failed",
                Some(self.ctx.i32_type().into()),
                &[cursor.into()],
            )?
            .unwrap()
            .into_int_value();
            let success = self.ctx.append_basic_block(function, "decode.success");
            let ok = builder
                .build_int_compare(
                    IntPredicate::EQ,
                    status,
                    self.ctx.i32_type().const_zero(),
                    "codec.complete",
                )
                .llvm_ctx("check complete decode")?;
            builder
                .build_conditional_branch(ok, success, rollback)
                .llvm_ctx("publish only valid payload")?;
            builder.position_at_end(success);
            if !reply {
                builder
                    .build_store(output, self.ctx.i8_type().const_int(1, false))
                    .llvm_ctx("publish active message payload")?;
            }
            builder
                .build_store(
                    function.get_nth_param(2).unwrap().into_pointer_value(),
                    size_ty.const_int(byte_size, false),
                )
                .llvm_ctx("publish decoded payload size")?;
            runtime("hew_cbor_de_free", None, &[cursor.into()])?;
            builder
                .build_return(Some(&output))
                .llvm_ctx("return decoded actor payload")?;
        } else {
            if !reply {
                runtime("hew_cbor_ser_end_array", None, &[cursor.into()])?;
            }
            let bytes = runtime(
                "hew_cbor_ser_finish",
                Some(ptr.into()),
                &[cursor.into(), function.get_nth_param(1).unwrap()],
            )?
            .unwrap();
            builder
                .build_return(Some(&bytes))
                .llvm_ctx("return encoded actor payload")?;
        }
        builder.position_at_end(rollback);
        if decode {
            let values = ValueEmitter {
                module: self.module,
                ctx: self.ctx,
                llvm: &self.llvm,
                builder: &builder,
                value: function,
                fault_sink: None,
            };
            for (slot, live, plan) in owners.iter().rev() {
                if let Some(action) = self.module.actor_recipes[&plan.ty].destroy {
                    let release = self.ctx.append_basic_block(function, "decode.release");
                    let next = self.ctx.append_basic_block(function, "decode.released");
                    let initialized = builder
                        .build_load(self.ctx.bool_type(), *live, "codec.initialized")
                        .llvm_ctx("inspect decoded owner")?
                        .into_int_value();
                    builder
                        .build_conditional_branch(initialized, release, next)
                        .llvm_ctx("release initialized fields")?;
                    builder.position_at_end(release);
                    let layout = self.module.target.layout(&plan.ty).ok_or_else(|| {
                        CodegenError::FailClosed("decoded owner lacks layout".into())
                    })?;
                    let value = builder
                        .build_load(llvm_type(self.ctx, &layout.repr)?, *slot, "codec.owner")
                        .llvm_ctx("load decoded owner")?;
                    values.destroy_loaded_value(value, layout, action)?;
                    builder
                        .build_unconditional_branch(next)
                        .llvm_ctx("continue decode rollback")?;
                    builder.position_at_end(next);
                }
            }
            let failed = builder
                .build_load(ptr, fault, "codec.failure")
                .llvm_ctx("load decode fault")?;
            runtime("hew_fault_drop", None, &[failed])?;
            runtime("hew_actor_payload_free", None, &[output.into()])?;
            runtime("hew_cbor_de_free", None, &[cursor.into()])?;
            builder
                .build_store(
                    function.get_nth_param(2).unwrap().into_pointer_value(),
                    size_ty.const_zero(),
                )
                .llvm_ctx("clear rejected payload size")?;
            builder
                .build_return(Some(&ptr.const_null()))
                .llvm_ctx("reject malformed actor payload")?;
        } else {
            builder
                .build_unreachable()
                .llvm_ctx("seal unused encode rollback")?;
        }
        Ok(function)
    }
}
