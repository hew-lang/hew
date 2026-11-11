//! Materialize a borrowed runtime failure as ordinary owned Hew values.

use super::*;
use hew_mir::RuntimeVariantRole;

impl<'ctx> ValueEmitter<'_, 'ctx> {
    fn failure_call(
        &self,
        name: &str,
        result: inkwell::types::BasicTypeEnum<'ctx>,
        snapshot: PointerValue<'ctx>,
        frame: Option<IntValue<'ctx>>,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let mut types = vec![self.ctx.ptr_type(AddressSpace::default()).into()];
        let mut args = vec![snapshot.into()];
        if let Some(frame) = frame {
            types.push(self.ctx.i32_type().into());
            args.push(frame.into());
        }
        let function = get_or_declare_external(self.llvm, name, result.fn_type(&types, false))?;
        call_value(self.builder, function, &args, "failure.field")
    }

    fn failure_option(&self, ty: &ResolvedTy) -> CodegenResult<(&PhysicalVariantGlue, u32, u32)> {
        let glue = self
            .module
            .variant_glue
            .iter()
            .find(|glue| glue.ty == *ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("failure option lacks a variant recipe".into())
            })?;
        let some = glue
            .runtime_tag(RuntimeVariantRole::OptionSome)
            .ok_or_else(|| CodegenError::FailClosed("failure option lacks Some".into()))?;
        let none = glue
            .runtime_tag(RuntimeVariantRole::OptionNone)
            .ok_or_else(|| CodegenError::FailClosed("failure option lacks None".into()))?;
        Ok((glue, some, none))
    }

    fn failure_site(
        &self,
        option_ty: &ResolvedTy,
        destination: PointerValue<'ctx>,
        snapshot: PointerValue<'ctx>,
        frame: Option<IntValue<'ctx>>,
    ) -> CodegenResult<()> {
        let (option, some, none) = self.failure_option(option_ty)?;
        let location_ty = &option.variants[some as usize].fields[0].ty;
        let layout =
            self.module.target.layout(location_ty).ok_or_else(|| {
                CodegenError::FailClosed("failure source lacks its layout".into())
            })?;
        let object = llvm_type(self.ctx, &layout.repr)?.into_struct_type();
        let slot = self.entry_scratch(object.into(), "failure.location")?;
        let present = self
            .failure_call(
                if frame.is_some() {
                    "hew_failure_snapshot_frame_has_site"
                } else {
                    "hew_failure_snapshot_has_site"
                },
                self.ctx.bool_type().into(),
                snapshot,
                frame,
            )?
            .into_int_value();
        let yes = self.ctx.append_basic_block(self.value, "failure.site.some");
        let no = self.ctx.append_basic_block(self.value, "failure.site.none");
        let done = self.ctx.append_basic_block(self.value, "failure.site.done");
        self.builder
            .build_conditional_branch(present, yes, no)
            .llvm_ctx("select failure source")?;
        self.builder.position_at_end(no);
        self.write_variant_value(destination, none, &[], option.id)?;
        self.builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish absent failure source")?;
        self.builder.position_at_end(yes);
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let file = self.failure_call(
            if frame.is_some() {
                "hew_failure_snapshot_frame_file"
            } else {
                "hew_failure_snapshot_site_file"
            },
            pointer.into(),
            snapshot,
            frame,
        )?;
        let copied = self
            .builder
            .build_is_not_null(file.into_pointer_value(), "failure.site.copied")
            .llvm_ctx("check copied source identity")?;
        let have_file = self
            .ctx
            .append_basic_block(self.value, "failure.site.copied");
        self.builder
            .build_conditional_branch(copied, have_file, no)
            .llvm_ctx("omit unavailable source identity")?;
        self.builder.position_at_end(have_file);
        let line = self.failure_call(
            if frame.is_some() {
                "hew_failure_snapshot_frame_line"
            } else {
                "hew_failure_snapshot_site_line"
            },
            self.ctx.i64_type().into(),
            snapshot,
            frame,
        )?;
        let column: BasicValueEnum<'ctx> = if frame.is_some() {
            self.ctx.i64_type().const_zero().into()
        } else {
            self.failure_call(
                "hew_failure_snapshot_site_column",
                self.ctx.i64_type().into(),
                snapshot,
                None,
            )?
        };
        for (index, field) in [file, line, column].into_iter().enumerate() {
            let address = self
                .builder
                .build_struct_gep(object, slot, index as u32, "failure.location.field")
                .llvm_ctx("address source field")?;
            self.builder
                .build_store(address, field)
                .llvm_ctx("store owned source field")?;
        }
        let value = self.load_value(slot, layout, "failure.location.value")?;
        self.write_variant_value(destination, some, &[value], option.id)?;
        self.builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish failure source")?;
        self.builder.position_at_end(done);
        Ok(())
    }

    fn failure_trace(
        &self,
        option_ty: &ResolvedTy,
        destination: PointerValue<'ctx>,
        snapshot: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let (option, some, none) = self.failure_option(option_ty)?;
        let vector_ty = &option.variants[some as usize].fields[0].ty;
        let vector = self
            .module
            .vector_glue
            .iter()
            .find(|glue| glue.ty == *vector_ty)
            .ok_or_else(|| CodegenError::FailClosed("failure trace lacks vector recipe".into()))?;
        let frame_ty = &vector.element.ty;
        let frame_layout = self
            .module
            .target
            .layout(frame_ty)
            .ok_or_else(|| CodegenError::FailClosed("failure frame lacks its layout".into()))?;
        let frame_glue = self
            .module
            .aggregate_glue
            .iter()
            .find(|glue| glue.ty == *frame_ty)
            .ok_or_else(|| CodegenError::FailClosed("failure frame lacks its recipe".into()))?;
        let frame_object = llvm_type(self.ctx, &frame_layout.repr)?.into_struct_type();
        let frame_slot = self.entry_scratch(frame_object.into(), "failure.frame")?;
        let count = self
            .failure_call(
                "hew_failure_snapshot_frame_count",
                self.ctx.i32_type().into(),
                snapshot,
                None,
            )?
            .into_int_value();
        let present = self
            .builder
            .build_int_compare(
                IntPredicate::SGE,
                count,
                self.ctx.i32_type().const_zero(),
                "failure.trace.present",
            )
            .llvm_ctx("test captured trace")?;
        let yes = self
            .ctx
            .append_basic_block(self.value, "failure.trace.some");
        let no = self
            .ctx
            .append_basic_block(self.value, "failure.trace.none");
        let loop_block = self
            .ctx
            .append_basic_block(self.value, "failure.trace.loop");
        let body = self
            .ctx
            .append_basic_block(self.value, "failure.trace.frame");
        let complete = self
            .ctx
            .append_basic_block(self.value, "failure.trace.complete");
        let done = self
            .ctx
            .append_basic_block(self.value, "failure.trace.done");
        self.builder
            .build_conditional_branch(present, yes, no)
            .llvm_ctx("select captured trace")?;
        self.builder.position_at_end(no);
        self.write_variant_value(destination, none, &[], option.id)?;
        self.builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish absent trace")?;
        self.builder.position_at_end(yes);
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let descriptor = self
            .llvm
            .get_global(&vector_descriptor_symbol(vector.id))
            .ok_or_else(|| {
                CodegenError::FailClosed("failure trace lacks element descriptor".into())
            })?;
        let constructor = get_or_declare_external(
            self.llvm,
            "hew_vec_try_new_with_elem_layout_capacity",
            pointer.fn_type(&[pointer.into(), self.ctx.i64_type().into()], false),
        )?;
        let capacity = self
            .builder
            .build_int_z_extend(count, self.ctx.i64_type(), "failure.trace.capacity")
            .llvm_ctx("extend trace length")?;
        let output = call_value(
            self.builder,
            constructor,
            &[descriptor.as_pointer_value().into(), capacity.into()],
            "failure.trace.vector",
        )?
        .into_pointer_value();
        let initial = self
            .builder
            .get_insert_block()
            .expect("trace constructor block");
        let allocated = self
            .builder
            .build_is_not_null(output, "failure.trace.allocated")
            .llvm_ctx("check trace allocation")?;
        self.builder
            .build_conditional_branch(allocated, loop_block, no)
            .llvm_ctx("omit an unavailable trace")?;
        self.builder.position_at_end(loop_block);
        let index = self
            .builder
            .build_phi(self.ctx.i32_type(), "failure.trace.index")
            .llvm_ctx("trace index")?;
        index.add_incoming(&[(&self.ctx.i32_type().const_zero(), initial)]);
        let current = index.as_basic_value().into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, current, count, "failure.trace.more")
            .llvm_ctx("bound trace index")?;
        self.builder
            .build_conditional_branch(more, body, complete)
            .llvm_ctx("select trace frame")?;
        self.builder.position_at_end(body);
        let function = self.failure_call(
            "hew_failure_snapshot_frame_function",
            pointer.into(),
            snapshot,
            Some(current),
        )?;
        let named = self
            .builder
            .build_is_not_null(function.into_pointer_value(), "failure.frame.named")
            .llvm_ctx("check frame name allocation")?;
        let frame_ready = self
            .ctx
            .append_basic_block(self.value, "failure.frame.ready");
        let abandon = self
            .ctx
            .append_basic_block(self.value, "failure.trace.abandon");
        self.builder
            .build_conditional_branch(named, frame_ready, abandon)
            .llvm_ctx("omit trace when a frame cannot be copied")?;
        self.builder.position_at_end(abandon);
        let free = get_or_declare_external(
            self.llvm,
            "hew_failure_free_unaccounted_trace",
            self.ctx.void_type().fn_type(&[pointer.into()], false),
        )?;
        self.builder
            .build_call(free, &[output.into()], "")
            .llvm_ctx("release partial trace")?;
        self.write_variant_value(destination, none, &[], option.id)?;
        self.builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish unavailable trace")?;
        self.builder.position_at_end(frame_ready);
        let function_slot = self
            .builder
            .build_struct_gep(frame_object, frame_slot, 0, "failure.frame.function")
            .llvm_ctx("address frame function")?;
        self.builder
            .build_store(function_slot, function)
            .llvm_ctx("store owned frame function")?;
        let site_slot = self
            .builder
            .build_struct_gep(frame_object, frame_slot, 1, "failure.frame.site")
            .llvm_ctx("address frame source")?;
        self.failure_site(&frame_glue.fields[1].ty, site_slot, snapshot, Some(current))?;
        let repeat = self.failure_call(
            "hew_failure_snapshot_frame_repeat",
            self.ctx.i64_type().into(),
            snapshot,
            Some(current),
        )?;
        let repeat_slot = self
            .builder
            .build_struct_gep(frame_object, frame_slot, 2, "failure.frame.repeat")
            .llvm_ctx("address frame repetition")?;
        self.builder
            .build_store(repeat_slot, repeat)
            .llvm_ctx("store frame repetition")?;
        let push = get_or_declare_external(
            self.llvm,
            "hew_vec_push_owned_move",
            self.ctx
                .void_type()
                .fn_type(&[pointer.into(), pointer.into()], false),
        )?;
        self.builder
            .build_call(push, &[output.into(), frame_slot.into()], "")
            .llvm_ctx("move frame into trace")?;
        let next = self
            .builder
            .build_int_add(
                current,
                self.ctx.i32_type().const_int(1, false),
                "failure.trace.next",
            )
            .llvm_ctx("advance trace")?;
        let back = self.builder.get_insert_block().expect("trace frame block");
        self.builder
            .build_unconditional_branch(loop_block)
            .llvm_ctx("continue trace")?;
        index.add_incoming(&[(&next, back)]);
        self.builder.position_at_end(complete);
        self.write_variant_value(destination, some, &[output.into()], option.id)?;
        self.builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish captured trace")?;
        self.builder.position_at_end(done);
        Ok(())
    }

    /// Build every public field from the same borrowed immutable failure.
    pub(super) fn emit_failure_info(
        &self,
        ty: &ResolvedTy,
        destination: PointerValue<'ctx>,
        snapshot: PointerValue<'ctx>,
        kind: IntValue<'ctx>,
    ) -> CodegenResult<()> {
        let info = self
            .module
            .aggregate_glue
            .iter()
            .find(|glue| glue.ty == *ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("CrashInfo lacks its aggregate recipe".into())
            })?;
        if info.fields.len() != 5 {
            return Err(CodegenError::FailClosed(
                "CrashInfo fields disagree with its runtime projection".into(),
            ));
        }
        let layout = self
            .module
            .target
            .layout(ty)
            .ok_or_else(|| CodegenError::FailClosed("CrashInfo lacks its layout".into()))?;
        let object = llvm_type(self.ctx, &layout.repr)?.into_struct_type();
        let field = |index, name| {
            self.builder
                .build_struct_gep(object, destination, index, name)
                .llvm_ctx("address CrashInfo field")
        };
        let kind_value = self.actor_unit_variant(&info.fields[0].ty, kind)?;
        self.builder
            .build_store(field(0, "failure.kind")?, kind_value)
            .llvm_ctx("store crash kind")?;
        let (trap, some, none) = self.failure_option(&info.fields[1].ty)?;
        let trap_ty = &trap.variants[some as usize].fields[0].ty;
        let tag = self
            .failure_call(
                "hew_failure_snapshot_trap",
                self.ctx.i32_type().into(),
                snapshot,
                None,
            )?
            .into_int_value();
        let known = self
            .builder
            .build_int_compare(
                IntPredicate::SGE,
                tag,
                self.ctx.i32_type().const_zero(),
                "failure.trap.known",
            )
            .llvm_ctx("test known trap")?;
        let yes = self.ctx.append_basic_block(self.value, "failure.trap.some");
        let no = self.ctx.append_basic_block(self.value, "failure.trap.none");
        let done = self.ctx.append_basic_block(self.value, "failure.trap.done");
        let trap_slot = field(1, "failure.trap")?;
        self.builder
            .build_conditional_branch(known, yes, no)
            .llvm_ctx("select trap")?;
        self.builder.position_at_end(no);
        self.write_variant_value(trap_slot, none, &[], trap.id)?;
        self.builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish absent trap")?;
        self.builder.position_at_end(yes);
        let value = self.actor_unit_variant(trap_ty, tag)?;
        self.write_variant_value(trap_slot, some, &[value], trap.id)?;
        self.builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish known trap")?;
        self.builder.position_at_end(done);
        let message = self.failure_call(
            "hew_failure_snapshot_message",
            self.ctx.ptr_type(AddressSpace::default()).into(),
            snapshot,
            None,
        )?;
        self.builder
            .build_store(field(2, "failure.message")?, message)
            .llvm_ctx("store failure message")?;
        self.failure_site(
            &info.fields[3].ty,
            field(3, "failure.site")?,
            snapshot,
            None,
        )?;
        self.failure_trace(&info.fields[4].ty, field(4, "failure.trace")?, snapshot)?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let descriptor = self
            .llvm
            .get_global(&super::super::managed::value_layout_symbol(
                self.module,
                ty,
            )?)
            .ok_or_else(|| {
                CodegenError::FailClosed("CrashInfo lacks its managed descriptor".into())
            })?;
        let adopt = get_or_declare_external(
            self.llvm,
            "hew_failure_try_adopt",
            self.ctx.bool_type().fn_type(&[pointer.into(); 2], false),
        )?;
        let accepted = call_value(
            self.builder,
            adopt,
            &[descriptor.as_pointer_value().into(), destination.into()],
            "failure.adopted",
        )?
        .into_int_value();
        let trace_refused = self
            .ctx
            .append_basic_block(self.value, "failure.adoption.trace_refused");
        let site_refused = self
            .ctx
            .append_basic_block(self.value, "failure.adoption.site_refused");
        let message_refused = self
            .ctx
            .append_basic_block(self.value, "failure.adoption.message_refused");
        let finished = self
            .ctx
            .append_basic_block(self.value, "failure.adoption.finished");
        self.builder
            .build_conditional_branch(accepted, finished, trace_refused)
            .llvm_ctx("adopt failure information")?;
        self.builder.position_at_end(trace_refused);
        let drop = get_or_declare_external(
            self.llvm,
            "hew_failure_drop_unaccounted",
            self.ctx.void_type().fn_type(&[pointer.into(); 2], false),
        )?;
        let (trace, _, trace_none) = self.failure_option(&info.fields[4].ty)?;
        let trace_slot = field(4, "failure.refused.trace")?;
        let trace_descriptor = self
            .llvm
            .get_global(&super::super::managed::value_layout_symbol(
                self.module,
                &info.fields[4].ty,
            )?)
            .ok_or_else(|| {
                CodegenError::FailClosed("failure trace lacks its managed descriptor".into())
            })?;
        self.builder
            .build_call(
                drop,
                &[
                    trace_descriptor.as_pointer_value().into(),
                    trace_slot.into(),
                ],
                "",
            )
            .llvm_ctx("discard unaccounted trace")?;
        self.write_variant_value(trace_slot, trace_none, &[], trace.id)?;
        let accepted = call_value(
            self.builder,
            adopt,
            &[descriptor.as_pointer_value().into(), destination.into()],
            "failure.base.adopted",
        )?
        .into_int_value();
        self.builder
            .build_conditional_branch(accepted, finished, site_refused)
            .llvm_ctx("retry without trace")?;
        self.builder.position_at_end(site_refused);
        let (site, _, site_none) = self.failure_option(&info.fields[3].ty)?;
        let site_slot = field(3, "failure.refused.site")?;
        let site_descriptor = self
            .llvm
            .get_global(&super::super::managed::value_layout_symbol(
                self.module,
                &info.fields[3].ty,
            )?)
            .ok_or_else(|| {
                CodegenError::FailClosed("failure site lacks its managed descriptor".into())
            })?;
        self.builder
            .build_call(
                drop,
                &[site_descriptor.as_pointer_value().into(), site_slot.into()],
                "",
            )
            .llvm_ctx("discard unaccounted source site")?;
        self.write_variant_value(site_slot, site_none, &[], site.id)?;
        let accepted = call_value(
            self.builder,
            adopt,
            &[descriptor.as_pointer_value().into(), destination.into()],
            "failure.message.adopted",
        )?
        .into_int_value();
        self.builder
            .build_conditional_branch(accepted, finished, message_refused)
            .llvm_ctx("retry without source site")?;
        self.builder.position_at_end(message_refused);
        let message_slot = field(2, "failure.refused.message")?;
        let message = self
            .builder
            .build_load(pointer, message_slot, "failure.refused.message.value")
            .llvm_ctx("read unaccounted message")?;
        let release = get_or_declare_external(
            self.llvm,
            "hew_string_drop",
            self.ctx.void_type().fn_type(&[pointer.into()], false),
        )?;
        self.builder
            .build_call(release, &[message.into()], "")
            .llvm_ctx("release unaccounted message")?;
        self.builder
            .build_store(message_slot, pointer.const_null())
            .llvm_ctx("empty unavailable message")?;
        self.builder
            .build_unconditional_branch(finished)
            .llvm_ctx("finish minimal failure")?;
        self.builder.position_at_end(finished);
        Ok(())
    }
}
