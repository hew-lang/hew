//! Structural rendering: `f"{v:?}"` on the physical path.
//!
//! Physical MIR selects the operand's rendering recipe; this emitter realizes
//! one borrow-only thunk per recipe between the builder's `new` and `finish`.
//! A rendered value stays borrowed throughout. Recipes that reach a suspending
//! Display body use the shared child-continuation protocol, including collection
//! traversal, so the actor worker can resume other turns. Failure or cancellation
//! stops traversal and carries the fault back to the source call's cleanup edge.

use super::*;
use hew_mir::physical::{PhysicalStructuralGlue, PhysicalStructuralId, PhysicalStructuralShape};

fn structural_thunk_symbol(id: PhysicalStructuralId) -> String {
    format!("__hew_structural_format_{}", id.0)
}

impl FunctionEmitter<'_, '_> {
    /// Render the borrowed operand into one fresh owned string.
    pub(super) fn emit_structural_format(
        &self,
        glue: PhysicalStructuralId,
        transfers: &[ArgumentTransfer],
        result: StorageId,
        normal: &PhysicalEdge,
        failure: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        let source = transfers.first().map(argument_source).ok_or_else(|| {
            CodegenError::FailClosed("structural rendering lacks its operand".into())
        })?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let values = self.value_emitter();
        let thunk = values.structural_thunk(glue)?;
        let new =
            get_or_declare_external(self.llvm, "hew_string_builder_new", ptr.fn_type(&[], false))?;
        let builder = self
            .runtime_call_value(new, &[], "structural.builder")?
            .into_pointer_value();
        self.builder
            .build_store(self.active_fault, ptr.const_null())
            .llvm_ctx("clear structural fault")?;
        let arguments = [
            builder.into(),
            self.slots[source.0 as usize].into(),
            self.active_fault.into(),
        ];
        let status = if structural_glue(self.module, glue)?.is_resumable {
            let frame = self.frame.as_ref().ok_or_else(|| {
                CodegenError::FailClosed(
                    "suspending structural rendering requires a caller frame".into(),
                )
            })?;
            invoke_child(
                self.ctx,
                self.llvm,
                &self.builder,
                self.value,
                frame,
                thunk,
                &arguments,
            )?
        } else {
            self.runtime_call_value(thunk, &arguments, "structural.render")?
                .into_int_value()
        };
        let finish = get_or_declare_external(
            self.llvm,
            "hew_string_builder_finish",
            ptr.fn_type(&[ptr.into()], false),
        )?;
        let text = self.runtime_call_value(finish, &[builder.into()], "structural.text")?;
        let ok = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_zero(),
                "structural.ok",
            )
            .llvm_ctx("test structural outcome")?;
        let success = self
            .ctx
            .append_basic_block(self.value, "structural.success");
        let failed = self.ctx.append_basic_block(self.value, "structural.failed");
        self.builder
            .build_conditional_branch(ok, success, failed)
            .llvm_ctx("branch on structural outcome")?;
        self.builder.position_at_end(failed);
        values.release_rendered_text(text)?;
        self.builder
            .build_store(self.active_status, status)
            .llvm_ctx("retain structural failure status")?;
        self.emit_edge(failure.ok_or_else(|| {
            CodegenError::FailClosed("structural rendering lacks callback fault cleanup".into())
        })?)?;
        self.builder.position_at_end(success);
        self.store(result, text)?;
        self.emit_result_edge(Some(result), normal)
    }
}

impl<'ctx> ValueEmitter<'_, 'ctx> {
    /// Each recipe retains its checked suspension demand. Pure recipes use
    /// the same traversal and finish synchronously; resumable ones return a
    /// scheduler-owned continuation and preserve the caller's task context.
    fn structural_thunk(&self, id: PhysicalStructuralId) -> CodegenResult<FunctionValue<'ctx>> {
        let glue = structural_glue(self.module, id)?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let signature = if glue.is_resumable {
            ptr.fn_type(&[ptr.into(); 4], false)
        } else {
            self.ctx.i32_type().fn_type(&[ptr.into(); 3], false)
        };
        self.glue_function(
            &structural_thunk_symbol(id),
            signature,
            |values, function| {
                let frame = if glue.is_resumable {
                    Some(coro::begin(
                        values.ctx,
                        values.llvm,
                        values.builder,
                        function,
                        function.get_nth_param(3).unwrap().into_pointer_value(),
                    )?)
                } else {
                    None
                };
                let status = values
                    .builder
                    .build_alloca(values.ctx.i32_type(), "structural.status.slot")
                    .llvm_ctx("allocate rendering status")?;
                let finish = values.ctx.append_basic_block(function, "structural.finish");
                let map_iter = if matches!(glue.shape, PhysicalStructuralShape::Map { .. }) {
                    let slot = values
                        .builder
                        .build_alloca(ptr, "structural.map.cursor")
                        .llvm_ctx("allocate map cursor slot")?;
                    values
                        .builder
                        .build_store(slot, ptr.const_null())
                        .llvm_ctx("initialize map cursor slot")?;
                    Some(slot)
                } else {
                    None
                };
                let emitter = StructuralEmitter {
                    values,
                    frame,
                    finish,
                    status,
                    map_iter,
                };
                let builder = function.get_nth_param(0).unwrap().into_pointer_value();
                let value = function.get_nth_param(1).unwrap().into_pointer_value();
                emitter.emit_structural_shape(glue, builder, value)?;
                emitter.finish_with_status(values.ctx.i32_type().const_zero())?;
                values.builder.position_at_end(finish);
                if let Some(slot) = map_iter {
                    let cursor = values
                        .builder
                        .build_load(ptr, slot, "structural.map.cursor")
                        .llvm_ctx("load map cursor")?;
                    let free = get_or_declare_external(
                        values.llvm,
                        "hew_hashmap_iter_free_layout",
                        values.ctx.void_type().fn_type(&[ptr.into()], false),
                    )?;
                    values
                        .builder
                        .build_call(free, &[cursor.into()], "")
                        .llvm_ctx("release structural map cursor")?;
                }
                let status = values
                    .builder
                    .build_load(values.ctx.i32_type(), status, "structural.status")
                    .llvm_ctx("load rendering status")?;
                if let Some(frame) = &emitter.frame {
                    let finish_state = coro::external(
                        values.llvm,
                        "hew_coro_state_finish",
                        values
                            .ctx
                            .i32_type()
                            .fn_type(&[ptr.into(), values.ctx.i32_type().into()], false),
                    )?;
                    values
                        .builder
                        .build_call(finish_state, &[frame.state.into(), status.into()], "")
                        .llvm_ctx("publish rendering outcome")?;
                    values
                        .builder
                        .build_unconditional_branch(frame.finish)
                        .llvm_ctx("finish rendering continuation")?;
                } else {
                    values
                        .builder
                        .build_return(Some(&status))
                        .llvm_ctx("return rendering outcome")?;
                }
                Ok(())
            },
        )
    }

    fn release_rendered_text(&self, text: BasicValueEnum<'ctx>) -> CodegenResult<()> {
        let layout = self
            .module
            .target
            .layout(&ResolvedTy::String)
            .ok_or_else(|| {
                CodegenError::FailClosed("structural text has no string layout".into())
            })?;
        self.destroy_loaded_value(text, layout, DestroyAction::StringRelease)
    }
}

struct StructuralEmitter<'a, 'b, 'ctx> {
    values: &'a ValueEmitter<'b, 'ctx>,
    frame: Option<coro::Frame<'ctx>>,
    finish: BasicBlock<'ctx>,
    status: PointerValue<'ctx>,
    map_iter: Option<PointerValue<'ctx>>,
}

impl<'a, 'b, 'ctx> std::ops::Deref for StructuralEmitter<'a, 'b, 'ctx> {
    type Target = ValueEmitter<'b, 'ctx>;
    fn deref(&self) -> &Self::Target {
        self.values
    }
}

impl<'ctx> StructuralEmitter<'_, '_, 'ctx> {
    fn finish_with_status(&self, status: IntValue<'ctx>) -> CodegenResult<()> {
        self.builder
            .build_store(self.status, status)
            .llvm_ctx("retain rendering status")?;
        self.builder
            .build_unconditional_branch(self.finish)
            .llvm_ctx("clean up structural traversal")?;
        Ok(())
    }

    /// Append the compiler's own punctuation and source names.
    fn append_literal(&self, builder: PointerValue<'ctx>, text: &str) -> CodegenResult<()> {
        if text.is_empty() {
            return Ok(());
        }
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let len = u32::try_from(text.len())
            .map_err(|_| CodegenError::FailClosed("structural literal exceeds u32 bytes".into()))?;
        let global = self
            .builder
            .build_global_string_ptr(text, "structural.literal")
            .llvm_ctx("stage structural literal")?;
        let append = get_or_declare_external(
            self.llvm,
            "hew_string_builder_append_literal",
            self.ctx
                .void_type()
                .fn_type(&[ptr.into(), ptr.into(), self.ctx.i32_type().into()], false),
        )?;
        self.builder
            .build_call(
                append,
                &[
                    builder.into(),
                    global.as_pointer_value().into(),
                    self.ctx.i32_type().const_int(u64::from(len), false).into(),
                ],
                "",
            )
            .llvm_ctx("append structural literal")?;
        Ok(())
    }

    /// Call one runtime builder entry that takes the builder and one value.
    fn append_scalar(
        &self,
        symbol: &str,
        parameter: BasicMetadataTypeEnum<'ctx>,
        builder: PointerValue<'ctx>,
        value: BasicMetadataValueEnum<'ctx>,
    ) -> CodegenResult<()> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let append = get_or_declare_external(
            self.llvm,
            symbol,
            self.ctx
                .void_type()
                .fn_type(&[ptr.into(), parameter], false),
        )?;
        self.builder
            .build_call(append, &[builder.into(), value], "")
            .llvm_ctx("append structural scalar")?;
        Ok(())
    }

    fn structural_layout(&self, glue: &PhysicalStructuralGlue) -> CodegenResult<&PhysicalLayout> {
        self.module.target.layout(&glue.ty).ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "structural recipe for `{}` has no target layout",
                glue.ty.user_facing()
            ))
        })
    }

    /// Append one member through its own thunk, borrowing it in place.
    fn render_member(
        &self,
        member: PhysicalStructuralId,
        builder: PointerValue<'ctx>,
        value: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let thunk = self.structural_thunk(member)?;
        self.render_call(
            thunk,
            structural_glue(self.module, member)?.is_resumable,
            &[
                builder.into(),
                value.into(),
                self.structural_fault()?.into(),
            ],
        )
    }

    fn structural_fault(&self) -> CodegenResult<PointerValue<'ctx>> {
        self.value
            .get_nth_param(2)
            .map(BasicValueEnum::into_pointer_value)
            .ok_or_else(|| {
                CodegenError::FailClosed("structural thunk lacks its fault output".into())
            })
    }

    fn render_call(
        &self,
        function: FunctionValue<'ctx>,
        resumable: bool,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) -> CodegenResult<()> {
        let status = if resumable {
            let frame = self.frame.as_ref().ok_or_else(|| {
                CodegenError::FailClosed("structural child suspension lacks a parent frame".into())
            })?;
            invoke_child(
                self.ctx,
                self.llvm,
                self.builder,
                self.value,
                frame,
                function,
                args,
            )?
        } else {
            self.builder
                .build_call(function, args, "structural.status")
                .llvm_ctx("call structural formatter")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("structural formatter returned no status".into())
                })?
                .into_int_value()
        };
        let ok = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_zero(),
                "structural.ok",
            )
            .llvm_ctx("test structural member status")?;
        let success = self
            .ctx
            .append_basic_block(self.value, "structural.member.success");
        let failed = self
            .ctx
            .append_basic_block(self.value, "structural.member.failed");
        self.builder
            .build_conditional_branch(ok, success, failed)
            .llvm_ctx("branch on structural member status")?;
        self.builder.position_at_end(failed);
        self.finish_with_status(status)?;
        self.builder.position_at_end(success);
        Ok(())
    }

    fn render_display(
        &self,
        id: CallableId,
        builder: PointerValue<'ctx>,
        value: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let callee = callable(self.module, id)?;
        let function = self
            .llvm
            .get_function(&if callee.is_resumable {
                format!("{}$resume", emitted_symbol(self.module, callee))
            } else {
                emitted_symbol(self.module, callee)
            })
            .ok_or_else(|| {
                CodegenError::FailClosed("structural Display has no emitted body".into())
            })?;
        let parameter = callee.params.first().ok_or_else(|| {
            CodegenError::FailClosed("structural Display lacks its receiver".into())
        })?;
        let receiver: BasicMetadataValueEnum<'ctx> = match parameter.carrier {
            ParamCarrier::Indirect => value.into(),
            ParamCarrier::Direct => self
                .builder
                .build_load(
                    llvm_type(self.ctx, &parameter.layout.repr)?,
                    value,
                    "structural.display.receiver",
                )
                .llvm_ctx("load structural Display receiver")?
                .into(),
        };
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let output = self
            .builder
            .build_alloca(ptr, "structural.display.text")
            .llvm_ctx("allocate Display output")?;
        self.render_call(
            function,
            callee.is_resumable,
            &[receiver, output.into(), self.structural_fault()?.into()],
        )?;
        let text = self
            .builder
            .build_load(ptr, output, "structural.display.result")
            .llvm_ctx("load structural Display result")?;
        self.append_scalar(
            "hew_string_builder_append_string",
            ptr.into(),
            builder,
            text.into(),
        )?;
        self.release_rendered_text(text)
    }

    fn separator(&self, first: IntValue<'ctx>, builder: PointerValue<'ctx>) -> CodegenResult<()> {
        let append = self
            .ctx
            .append_basic_block(self.value, "structural.separator");
        let ready = self
            .ctx
            .append_basic_block(self.value, "structural.element");
        self.builder
            .build_conditional_branch(first, ready, append)
            .llvm_ctx("select collection separator")?;
        self.builder.position_at_end(append);
        self.append_literal(builder, ", ")?;
        self.builder
            .build_unconditional_branch(ready)
            .llvm_ctx("enter collection element")?;
        self.builder.position_at_end(ready);
        Ok(())
    }

    fn render_vector(
        &self,
        element: PhysicalStructuralId,
        builder: PointerValue<'ctx>,
        value: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let i64_ty = self.ctx.i64_type();
        let vector = self
            .builder
            .build_load(ptr, value, "structural.vector")
            .llvm_ctx("load structural vector")?;
        let len_fn = get_or_declare_external(
            self.llvm,
            "hew_vec_len",
            i64_ty.fn_type(&[ptr.into()], false),
        )?;
        let len = suspend::call_value(
            self.builder,
            len_fn,
            &[vector.into()],
            "structural.vector.len",
        )?
        .into_int_value();
        let layout = self.structural_layout(structural_glue(self.module, element)?)?;
        let slot = self
            .builder
            .build_alloca(
                llvm_type(self.ctx, &layout.repr)?,
                "structural.element.borrow",
            )
            .llvm_ctx("allocate borrowed element slot")?;
        let index_slot = self
            .builder
            .build_alloca(i64_ty, "structural.index")
            .llvm_ctx("allocate vector index")?;
        self.builder
            .build_store(index_slot, i64_ty.const_zero())
            .llvm_ctx("initialize vector index")?;
        self.append_literal(builder, "[")?;
        let poll = self
            .ctx
            .append_basic_block(self.value, "structural.vector.next");
        let item = self
            .ctx
            .append_basic_block(self.value, "structural.vector.item");
        let done = self
            .ctx
            .append_basic_block(self.value, "structural.vector.done");
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("start vector rendering")?;
        self.builder.position_at_end(poll);
        let index = self
            .builder
            .build_load(i64_ty, index_slot, "structural.index")
            .llvm_ctx("load vector index")?
            .into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, index, len, "structural.vector.more")
            .llvm_ctx("test vector index")?;
        self.builder
            .build_conditional_branch(more, item, done)
            .llvm_ctx("select vector element")?;
        self.builder.position_at_end(item);
        let first = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                index,
                i64_ty.const_zero(),
                "structural.first",
            )
            .llvm_ctx("test first vector element")?;
        self.separator(first, builder)?;
        let borrow = get_or_declare_external(
            self.llvm,
            "hew_vec_borrow_owned",
            self.ctx
                .bool_type()
                .fn_type(&[ptr.into(), i64_ty.into(), ptr.into()], false),
        )?;
        self.builder
            .build_call(borrow, &[vector.into(), index.into(), slot.into()], "")
            .llvm_ctx("borrow structural vector element")?;
        self.render_member(element, builder, slot)?;
        let next = self
            .builder
            .build_int_add(index, i64_ty.const_int(1, false), "structural.next.index")
            .llvm_ctx("advance vector index")?;
        self.builder
            .build_store(index_slot, next)
            .llvm_ctx("retain vector index")?;
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("continue vector rendering")?;
        self.builder.position_at_end(done);
        self.append_literal(builder, "]")
    }

    fn render_map(
        &self,
        key: PhysicalStructuralId,
        entry: PhysicalStructuralId,
        builder: PointerValue<'ctx>,
        value: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let map = self
            .builder
            .build_load(ptr, value, "structural.map")
            .llvm_ctx("load structural map")?
            .into_pointer_value();
        self.append_literal(builder, "{")?;
        let create = self
            .ctx
            .append_basic_block(self.value, "structural.map.begin");
        let poll = self
            .ctx
            .append_basic_block(self.value, "structural.map.next");
        let item = self
            .ctx
            .append_basic_block(self.value, "structural.map.item");
        let done = self
            .ctx
            .append_basic_block(self.value, "structural.map.done");
        let absent = self
            .builder
            .build_is_null(map, "structural.map.absent")
            .llvm_ctx("test empty map")?;
        self.builder
            .build_conditional_branch(absent, done, create)
            .llvm_ctx("select map traversal")?;
        self.builder.position_at_end(create);
        let iter_new = get_or_declare_external(
            self.llvm,
            "hew_hashmap_iter_new_layout",
            ptr.fn_type(&[ptr.into()], false),
        )?;
        let cursor = suspend::call_value(
            self.builder,
            iter_new,
            &[map.into()],
            "structural.map.cursor",
        )?;
        self.builder
            .build_store(
                self.map_iter.ok_or_else(|| {
                    CodegenError::FailClosed("map rendering lacks cursor cleanup storage".into())
                })?,
                cursor,
            )
            .llvm_ctx("retain map cursor")?;
        let key_slot = self
            .builder
            .build_alloca(ptr, "structural.map.key")
            .llvm_ctx("allocate borrowed key slot")?;
        let value_slot = self
            .builder
            .build_alloca(ptr, "structural.map.value")
            .llvm_ctx("allocate borrowed value slot")?;
        let first_slot = self
            .builder
            .build_alloca(self.ctx.bool_type(), "structural.map.first")
            .llvm_ctx("allocate map separator state")?;
        self.builder
            .build_store(first_slot, self.ctx.bool_type().const_int(1, false))
            .llvm_ctx("initialize map separator state")?;
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("start map iteration")?;
        self.builder.position_at_end(poll);
        let next = get_or_declare_external(
            self.llvm,
            "hew_hashmap_iter_next_layout",
            self.ctx.bool_type().fn_type(&[ptr.into(); 3], false),
        )?;
        let more = suspend::call_value(
            self.builder,
            next,
            &[cursor.into(), key_slot.into(), value_slot.into()],
            "structural.map.more",
        )?
        .into_int_value();
        self.builder
            .build_conditional_branch(more, item, done)
            .llvm_ctx("select map entry")?;
        self.builder.position_at_end(item);
        let first = self
            .builder
            .build_load(self.ctx.bool_type(), first_slot, "structural.map.first")
            .llvm_ctx("load map separator state")?
            .into_int_value();
        self.separator(first, builder)?;
        self.builder
            .build_store(first_slot, self.ctx.bool_type().const_zero())
            .llvm_ctx("advance map separator state")?;
        let key_value = self
            .builder
            .build_load(ptr, key_slot, "structural.map.key")
            .llvm_ctx("load borrowed map key")?
            .into_pointer_value();
        self.render_member(key, builder, key_value)?;
        self.append_literal(builder, ": ")?;
        let entry_value = self
            .builder
            .build_load(ptr, value_slot, "structural.map.value")
            .llvm_ctx("load borrowed map value")?
            .into_pointer_value();
        self.render_member(entry, builder, entry_value)?;
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("continue map rendering")?;
        self.builder.position_at_end(done);
        self.append_literal(builder, "}")
    }

    #[expect(
        clippy::too_many_lines,
        reason = "one arm per rendering shape; splitting them hides the spelling"
    )]
    fn emit_structural_shape(
        &self,
        glue: &PhysicalStructuralGlue,
        builder: PointerValue<'ctx>,
        value: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let i64_ty = self.ctx.i64_type();
        match &glue.shape {
            PhysicalStructuralShape::Display { callable } => {
                self.render_display(*callable, builder, value)
            }
            PhysicalStructuralShape::Unit => self.append_literal(builder, "()"),
            PhysicalStructuralShape::SignedInt | PhysicalStructuralShape::UnsignedInt => {
                let layout = self.structural_layout(glue)?;
                let loaded = self
                    .builder
                    .build_load(llvm_type(self.ctx, &layout.repr)?, value, "structural.int")
                    .llvm_ctx("read structural integer")?
                    .into_int_value();
                let (symbol, widened) = if matches!(glue.shape, PhysicalStructuralShape::SignedInt)
                {
                    (
                        "hew_string_builder_append_i64",
                        self.builder
                            .build_int_s_extend_or_bit_cast(loaded, i64_ty, "structural.sext")
                            .llvm_ctx("widen signed structural integer")?,
                    )
                } else {
                    (
                        "hew_string_builder_append_u64",
                        self.builder
                            .build_int_z_extend_or_bit_cast(loaded, i64_ty, "structural.zext")
                            .llvm_ctx("widen unsigned structural integer")?,
                    )
                };
                self.append_scalar(symbol, i64_ty.into(), builder, widened.into())
            }
            PhysicalStructuralShape::Float => {
                let layout = self.structural_layout(glue)?;
                let loaded = self
                    .builder
                    .build_load(
                        llvm_type(self.ctx, &layout.repr)?,
                        value,
                        "structural.float",
                    )
                    .llvm_ctx("read structural float")?
                    .into_float_value();
                let widened = self
                    .builder
                    .build_float_cast(loaded, self.ctx.f64_type(), "structural.fpext")
                    .llvm_ctx("widen structural float")?;
                self.append_scalar(
                    "hew_string_builder_append_f64",
                    self.ctx.f64_type().into(),
                    builder,
                    widened.into(),
                )
            }
            PhysicalStructuralShape::Bool => {
                let layout = self.structural_layout(glue)?;
                let loaded = self
                    .builder
                    .build_load(llvm_type(self.ctx, &layout.repr)?, value, "structural.bool")
                    .llvm_ctx("read structural boolean")?
                    .into_int_value();
                let byte = self
                    .builder
                    .build_int_cast(loaded, self.ctx.i8_type(), "structural.bool.byte")
                    .llvm_ctx("narrow structural boolean")?;
                self.append_scalar(
                    "hew_string_builder_append_bool",
                    self.ctx.i8_type().into(),
                    builder,
                    byte.into(),
                )
            }
            PhysicalStructuralShape::Char => {
                let layout = self.structural_layout(glue)?;
                let loaded = self
                    .builder
                    .build_load(llvm_type(self.ctx, &layout.repr)?, value, "structural.char")
                    .llvm_ctx("read structural character")?
                    .into_int_value();
                let scalar = self
                    .builder
                    .build_int_cast(loaded, self.ctx.i32_type(), "structural.char.scalar")
                    .llvm_ctx("widen structural character")?;
                self.append_scalar(
                    "hew_string_builder_append_char",
                    self.ctx.i32_type().into(),
                    builder,
                    scalar.into(),
                )
            }
            PhysicalStructuralShape::String => {
                let loaded = self
                    .builder
                    .build_load(ptr, value, "structural.string")
                    .llvm_ctx("read structural string")?;
                self.append_scalar(
                    "hew_string_builder_append_string",
                    ptr.into(),
                    builder,
                    loaded.into(),
                )
            }
            PhysicalStructuralShape::Identity { name } => {
                let layout = self.structural_layout(glue)?;
                let loaded = self
                    .builder
                    .build_load(
                        llvm_type(self.ctx, &layout.repr)?,
                        value,
                        "structural.identity",
                    )
                    .llvm_ctx("read structural identity")?;
                // An opaque handle rides a pointer or a pointer-sized integer;
                // either way only its identity is disclosed.
                let identity = match loaded {
                    BasicValueEnum::PointerValue(handle) => handle,
                    BasicValueEnum::IntValue(bits) => self
                        .builder
                        .build_int_to_ptr(bits, ptr, "structural.identity.ptr")
                        .llvm_ctx("carry structural identity bits")?,
                    _ => {
                        return Err(CodegenError::FailClosed(format!(
                            "opaque `{}` has no identity carrier",
                            glue.ty.user_facing()
                        )))
                    }
                };
                let type_name = self
                    .builder
                    .build_global_string_ptr(name, "structural.type.name")
                    .llvm_ctx("stage structural type name")?;
                let append = get_or_declare_external(
                    self.llvm,
                    "hew_string_builder_append_identity",
                    self.ctx
                        .void_type()
                        .fn_type(&[ptr.into(), ptr.into(), ptr.into()], false),
                )?;
                self.builder
                    .build_call(
                        append,
                        &[
                            builder.into(),
                            type_name.as_pointer_value().into(),
                            identity.into(),
                        ],
                        "",
                    )
                    .llvm_ctx("append structural identity")?;
                Ok(())
            }
            PhysicalStructuralShape::Tuple { fields } => {
                let layout = self.structural_layout(glue)?;
                let struct_ty = llvm_type(self.ctx, &layout.repr)?.into_struct_type();
                self.append_literal(builder, "(")?;
                for (index, member) in fields.iter().enumerate() {
                    if index > 0 {
                        self.append_literal(builder, ", ")?;
                    }
                    let index = u32::try_from(index).map_err(|_| {
                        CodegenError::FailClosed("structural tuple index exceeds u32".into())
                    })?;
                    let field = self
                        .builder
                        .build_struct_gep(struct_ty, value, index, "structural.tuple.field")
                        .llvm_ctx("address structural tuple member")?;
                    self.render_member(*member, builder, field)?;
                }
                self.append_literal(builder, ")")
            }
            PhysicalStructuralShape::Record { name, fields } => {
                if fields.is_empty() {
                    return self.append_literal(builder, name);
                }
                let layout = self.structural_layout(glue)?;
                let struct_ty = llvm_type(self.ctx, &layout.repr)?.into_struct_type();
                self.append_literal(builder, &format!("{name} {{ "))?;
                for (index, field) in fields.iter().enumerate() {
                    if index > 0 {
                        self.append_literal(builder, ", ")?;
                    }
                    self.append_literal(builder, &format!("{}: ", field.name))?;
                    let index = u32::try_from(index).map_err(|_| {
                        CodegenError::FailClosed("structural field index exceeds u32".into())
                    })?;
                    let member = self
                        .builder
                        .build_struct_gep(struct_ty, value, index, "structural.record.field")
                        .llvm_ctx("address structural record field")?;
                    self.render_member(field.recipe, builder, member)?;
                }
                self.append_literal(builder, " }")
            }
            PhysicalStructuralShape::Enum { cases } => {
                let layout = self.variant_layout(&glue.ty)?;
                let object = self.variant_object_ptr(value, layout)?;
                let tag = self.load_variant_tag(object, layout)?;
                let invalid = self
                    .ctx
                    .append_basic_block(self.value, "structural.enum.invalid");
                let complete = self
                    .ctx
                    .append_basic_block(self.value, "structural.enum.complete");
                let blocks = cases
                    .iter()
                    .enumerate()
                    .map(|(index, _)| {
                        (
                            tag.get_type().const_int(index as u64, false),
                            self.ctx.append_basic_block(
                                self.value,
                                &format!("structural.enum.case.{index}"),
                            ),
                        )
                    })
                    .collect::<Vec<_>>();
                self.builder
                    .build_switch(tag, invalid, &blocks)
                    .llvm_ctx("dispatch structural enum case")?;
                for (index, (_, block)) in blocks.iter().enumerate() {
                    self.builder.position_at_end(*block);
                    let case = &cases[index];
                    self.append_literal(builder, &case.name)?;
                    if case.kind != hew_mir::physical::SemVariantKind::Unit {
                        let payload_ty =
                            llvm_type(self.ctx, &layout.variants[index].repr)?.into_struct_type();
                        let payload = self.variant_payload_ptr(object, layout)?;
                        let named = case.kind == hew_mir::physical::SemVariantKind::Struct;
                        self.append_literal(builder, if named { " { " } else { "(" })?;
                        for (position, member) in case.fields.iter().enumerate() {
                            if position > 0 {
                                self.append_literal(builder, ", ")?;
                            }
                            if named {
                                self.append_literal(builder, &format!("{}: ", member.name))?;
                            }
                            let position = u32::try_from(position).map_err(|_| {
                                CodegenError::FailClosed(
                                    "structural payload index exceeds u32".into(),
                                )
                            })?;
                            let field = self
                                .builder
                                .build_struct_gep(
                                    payload_ty,
                                    payload,
                                    position,
                                    "structural.enum.field",
                                )
                                .llvm_ctx("address structural enum payload")?;
                            self.render_member(member.recipe, builder, field)?;
                        }
                        self.append_literal(builder, if named { " }" } else { ")" })?;
                    }
                    self.builder
                        .build_unconditional_branch(complete)
                        .llvm_ctx("finish structural enum case")?;
                }
                self.builder.position_at_end(invalid);
                self.emit_invalid_variant_tag()?;
                self.builder.position_at_end(complete);
                Ok(())
            }
            PhysicalStructuralShape::Vector { element } => {
                self.render_vector(*element, builder, value)
            }
            PhysicalStructuralShape::Map { key, value: entry } => {
                self.render_map(*key, *entry, builder, value)
            }
        }
    }
}

fn structural_glue(
    module: &PhysicalModule,
    id: PhysicalStructuralId,
) -> CodegenResult<&PhysicalStructuralGlue> {
    module
        .structural_glue
        .get(id.0 as usize)
        .filter(|glue| glue.id == id)
        .ok_or_else(|| CodegenError::FailClosed("unknown structural rendering recipe".into()))
}

fn invoke_child<'ctx>(
    ctx: &'ctx Context,
    llvm: &Module<'ctx>,
    builder: &Builder<'ctx>,
    function: FunctionValue<'ctx>,
    frame: &coro::Frame<'ctx>,
    callee: FunctionValue<'ctx>,
    args: &[BasicMetadataValueEnum<'ctx>],
) -> CodegenResult<IntValue<'ctx>> {
    let pointer = ctx.ptr_type(AddressSpace::default());
    let create = coro::external(
        llvm,
        "hew_coro_state_child",
        pointer.fn_type(&[pointer.into()], false),
    )?;
    let child = suspend::call_value(
        builder,
        create,
        &[frame.state.into()],
        "structural.child.state",
    )?
    .into_pointer_value();
    let mut args = args.to_vec();
    args.push(child.into());
    let child_frame =
        suspend::call_value(builder, callee, &args, "structural.child.frame")?.into_pointer_value();
    suspend::await_child(ctx, llvm, builder, function, frame, child, child_frame)
}
