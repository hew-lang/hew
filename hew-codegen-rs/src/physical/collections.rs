//! Array, vector, map and set operations.

use super::*;
use inkwell::values::BasicValue;

/// The C-ABI `HewVec` prefix used by checked element access.
pub(super) fn vector_header_type<'ctx>(
    ctx: &'ctx Context,
    target: &TargetData,
) -> inkwell::types::StructType<'ctx> {
    ctx.struct_type(
        &[
            ctx.ptr_type(AddressSpace::default()).into(),
            ctx.ptr_sized_int_type(target, None).into(),
        ],
        false,
    )
}

impl<'a, 'ctx> FunctionEmitter<'a, 'ctx> {
    fn make_vector_unique(&self, vector: PointerValue<'ctx>) -> CodegenResult<()> {
        let target = TargetData::create(&self.module.target.data_layout);
        let word = self.ctx.ptr_sized_int_type(&target, None);
        let data = self
            .builder
            .build_load(
                self.ctx.ptr_type(AddressSpace::default()),
                vector,
                "vector.data",
            )
            .llvm_ctx("load vector buffer before mutation")?
            .into_pointer_value();
        // SAFETY: the runtime buffer ABI places its atomic reference count one target
        // word before data. Bounds checking has proved a nonempty live buffer.
        let owner = unsafe {
            self.builder.build_in_bounds_gep(
                word,
                data,
                &[word.const_all_ones()],
                "vector.buffer.owner",
            )
        }
        .llvm_ctx("address vector buffer reference count")?;
        let references = self
            .builder
            .build_load(word, owner, "vector.buffer.references")
            .llvm_ctx("load vector buffer reference count")?;
        references
            .as_instruction_value()
            .ok_or_else(|| CodegenError::FailClosed("vector reference count is not a load".into()))?
            .set_atomic_ordering(inkwell::AtomicOrdering::Acquire)
            .llvm_ctx("synchronize vector buffer ownership")?;
        let unique = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                references.into_int_value(),
                word.const_int(1, false),
                "vector.buffer.unique",
            )
            .llvm_ctx("test vector buffer uniqueness")?;
        let ready = self
            .ctx
            .append_basic_block(self.value, "vector.mutation.ready");
        let detach = self
            .ctx
            .append_basic_block(self.value, "vector.buffer.detach");
        self.builder
            .build_conditional_branch(unique, ready, detach)
            .llvm_ctx("select vector buffer detachment")?;
        self.builder.position_at_end(detach);
        let function = get_or_declare_external(
            self.llvm,
            "hew_vec_make_unique",
            self.ctx
                .void_type()
                .fn_type(&[vector.get_type().into()], false),
        )?;
        self.runtime_call_void(function, &[vector.into()], "vector.make_unique")?;
        self.builder
            .build_unconditional_branch(ready)
            .llvm_ctx("continue with unique vector storage")?;
        self.builder.position_at_end(ready);
        Ok(())
    }

    fn vector_length(&self, vector: PointerValue<'ctx>) -> CodegenResult<IntValue<'ctx>> {
        let target = TargetData::create(&self.module.target.data_layout);
        let word = self.ctx.ptr_sized_int_type(&target, None);
        let slot = self
            .builder
            .build_struct_gep(
                vector_header_type(self.ctx, &target),
                vector,
                1,
                "vector.len.slot",
            )
            .llvm_ctx("address vector length")?;
        let length = self
            .builder
            .build_load(word, slot, "vector.len")
            .llvm_ctx("load vector length")?
            .into_int_value();
        self.builder
            .build_int_z_extend_or_bit_cast(length, self.ctx.i64_type(), "vector.length")
            .llvm_ctx("widen vector length")
    }

    fn vector_element_slot(
        &self,
        vector: PointerValue<'ctx>,
        index: IntValue<'ctx>,
        element: BasicTypeEnum<'ctx>,
    ) -> CodegenResult<PointerValue<'ctx>> {
        // `data` is the first C-ABI field. The checked target layout supplies
        // the element stride; no descriptor dispatch is needed for scalars.
        let data = self
            .builder
            .build_load(
                self.ctx.ptr_type(AddressSpace::default()),
                vector,
                "vector.data",
            )
            .llvm_ctx("load vector buffer")?
            .into_pointer_value();
        // SAFETY: the caller has checked index against the live vector length.
        unsafe {
            self.builder
                .build_in_bounds_gep(element, data, &[index], "vector.element")
                .llvm_ctx("address checked vector element")
        }
    }

    fn new_array_storage(
        &self,
        glue: PhysicalVectorId,
    ) -> CodegenResult<inkwell::values::PointerValue<'ctx>> {
        let values = self.value_emitter();
        let descriptor = values.vector_glue(glue)?;
        let ResolvedTy::Array(_, length) = &descriptor.ty else {
            return Err(CodegenError::FailClosed(
                "array construction has a non-array descriptor".into(),
            ));
        };
        let layout = self
            .llvm
            .get_global(&vector_descriptor_symbol(glue))
            .ok_or_else(|| {
                CodegenError::FailClosed("array element descriptor was not emitted".into())
            })?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let constructor = get_or_declare_external(
            self.llvm,
            "hew_vec_new_with_elem_layout_capacity",
            pointer.fn_type(&[pointer.into(), self.ctx.i64_type().into()], false),
        )?;
        Ok(self
            .runtime_call_value(
                constructor,
                &[
                    layout.as_pointer_value().into(),
                    self.ctx.i64_type().const_int(*length, false).into(),
                ],
                "array.storage",
            )?
            .into_pointer_value())
    }

    pub(super) fn emit_array_make(
        &self,
        dest: StorageId,
        fields: &[StorageId],
        glue: PhysicalVectorId,
    ) -> CodegenResult<()> {
        let array = self.new_array_storage(glue)?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let push = get_or_declare_external(
            self.llvm,
            "hew_vec_push_owned_move",
            self.ctx
                .void_type()
                .fn_type(&[pointer.into(), pointer.into()], false),
        )?;
        for field in fields {
            self.runtime_call_void(
                push,
                &[array.into(), self.slots[field.0 as usize].into()],
                "array.element",
            )?;
            self.clear_owned(*field)?;
        }
        self.store(dest, array.into())
    }

    pub(super) fn emit_array_repeat(
        &self,
        dest: StorageId,
        seed: StorageId,
        glue: PhysicalVectorId,
    ) -> CodegenResult<()> {
        let values = self.value_emitter();
        let descriptor = values.vector_glue(glue)?;
        let ResolvedTy::Array(_, length) = &descriptor.ty else {
            return Err(CodegenError::FailClosed(
                "array repeat has a non-array descriptor".into(),
            ));
        };
        let array = self.new_array_storage(glue)?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let push_ty = self
            .ctx
            .void_type()
            .fn_type(&[pointer.into(), pointer.into()], false);
        if *length > 1 {
            let push = get_or_declare_external(self.llvm, "hew_vec_push_owned", push_ty)?;
            let before = self.builder.get_insert_block().ok_or_else(|| {
                CodegenError::FailClosed("array repeat has no insertion block".into())
            })?;
            let body = self.ctx.append_basic_block(self.value, "array.repeat.copy");
            let done = self.ctx.append_basic_block(self.value, "array.repeat.done");
            self.builder
                .build_unconditional_branch(body)
                .llvm_ctx("enter array repeat")?;
            self.builder.position_at_end(body);
            let i64_ty = self.ctx.i64_type();
            let index = self
                .builder
                .build_phi(i64_ty, "array.repeat.index")
                .llvm_ctx("array repeat counter")?;
            index.add_incoming(&[(&i64_ty.const_zero(), before)]);
            self.runtime_call_void(
                push,
                &[array.into(), self.slots[seed.0 as usize].into()],
                "array.repeat.element",
            )?;
            let next = self
                .builder
                .build_int_add(
                    index.as_basic_value().into_int_value(),
                    i64_ty.const_int(1, false),
                    "array.repeat.next",
                )
                .llvm_ctx("advance array repeat")?;
            let more = self
                .builder
                .build_int_compare(
                    IntPredicate::ULT,
                    next,
                    i64_ty.const_int(*length - 1, false),
                    "array.repeat.more",
                )
                .llvm_ctx("check array repeat limit")?;
            self.builder
                .build_conditional_branch(more, body, done)
                .llvm_ctx("continue array repeat")?;
            index.add_incoming(&[(&next, body)]);
            self.builder.position_at_end(done);
        }
        let push = get_or_declare_external(self.llvm, "hew_vec_push_owned_move", push_ty)?;
        self.runtime_call_void(
            push,
            &[array.into(), self.slots[seed.0 as usize].into()],
            "array.repeat.last",
        )?;
        self.clear_owned(seed)?;
        self.store(dest, array.into())
    }

    /// Load the compiled `*HewRegex` for the literal slot named by `index`.
    /// Every regex action addresses the module's handle array this way; the
    /// patterns are compiled once when the native module loads.
    pub(super) fn load_regex_handle(
        &self,
        index: StorageId,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let count = regex_slot_count(self.module)?.ok_or_else(|| {
            CodegenError::FailClosed("a regex operation needs a compiled pattern slot".into())
        })?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let handles = regex_handles(self.llvm)?;
        let index = self.load(index, "regex.index")?.into_int_value();
        let slot = unsafe {
            self.builder
                .build_gep(
                    ptr.array_type(count),
                    handles.as_pointer_value(),
                    &[self.ctx.i64_type().const_zero(), index],
                    "regex.slot",
                )
                .llvm_ctx("address the compiled regex slot")?
        };
        self.builder
            .build_load(ptr, slot, "regex.handle")
            .llvm_ctx("load the compiled regex handle")
    }

    fn emit_vector_index_guard(
        &self,
        vector: PointerValue<'ctx>,
        index: IntValue<'ctx>,
        failure: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let length = self.vector_length(vector)?;
        // Unsigned comparison also rejects every negative signed index.
        let in_bounds = self
            .builder
            .build_int_compare(IntPredicate::ULT, index, length, "vector.index.in.bounds")
            .llvm_ctx("check vector mutation bounds")?;
        let safe = self.ctx.append_basic_block(self.value, "vector.index.safe");
        let failed = self
            .ctx
            .append_basic_block(self.value, "vector.index.failed");
        self.builder
            .build_conditional_branch(in_bounds, safe, failed)
            .llvm_ctx("select vector mutation outcome")?;
        self.builder.position_at_end(failed);
        self.emit_edge(failure)?;
        self.builder.position_at_end(safe);
        Ok(())
    }

    #[expect(
        clippy::too_many_lines,
        reason = "each vector action executes its checked storage and failure contract"
    )]
    pub(super) fn emit_vector_call(
        &self,
        action: (PhysicalVectorOp, PhysicalVectorId),
        transfers: &[ArgumentTransfer],
        result: StorageId,
        normal: &PhysicalEdge,
        failure: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        let (operation, glue_id) = action;
        let values = self.value_emitter();
        let glue = values.vector_glue(glue_id)?;
        let source = |index: usize| {
            transfers.get(index).map(argument_source).ok_or_else(|| {
                CodegenError::FailClosed(format!("vector action lacks argument {index}"))
            })
        };
        let failure = || {
            failure.ok_or_else(|| {
                CodegenError::FailClosed("fallible vector action lacks its cleanup edge".into())
            })
        };
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let i64_ty = self.ctx.i64_type();
        let element_layout = self.module.target.layout(&glue.element.ty).ok_or_else(|| {
            CodegenError::FailClosed("vector element lacks its target layout".into())
        })?;
        let scalar = glue.element.own == OwnKind::None
            && glue.element.clone == Some(CloneAction::Bitwise)
            && glue.element.destroy.is_none()
            && matches!(
                element_layout.repr,
                PhysicalRepr::Integer { .. } | PhysicalRepr::Float { .. }
            );
        if operation == PhysicalVectorOp::New {
            let descriptor = self
                .llvm
                .get_global(&vector_descriptor_symbol(glue_id))
                .ok_or_else(|| {
                    CodegenError::FailClosed("vector descriptor was not emitted".into())
                })?;
            let function = external_unary_ptr(self.ctx, self.llvm, "hew_vec_new_with_elem_layout")?;
            let value = self.runtime_call_value(
                function,
                &[descriptor.as_pointer_value().into()],
                "vector.new",
            )?;
            self.store(result, value)?;
            return self.emit_result_edge(Some(result), normal);
        }
        let receiver = source(0)?;
        let vector = self.load(receiver, "vector.receiver")?.into_pointer_value();
        match operation {
            PhysicalVectorOp::New => unreachable!("new handled before receiver loading"),
            PhysicalVectorOp::Len => {
                self.store(result, self.vector_length(vector)?.into())?;
            }
            PhysicalVectorOp::Contains => {
                return self.emit_vector_contains(
                    &glue.element.ty,
                    vector,
                    self.slots[source(1)?.0 as usize],
                    result,
                    normal,
                    failure()?,
                );
            }
            PhysicalVectorOp::Push | PhysicalVectorOp::Clear => {
                if operation == PhysicalVectorOp::Push {
                    let moved = matches!(transfers.get(1), Some(ArgumentTransfer::Move(_)));
                    let function = get_or_declare_external(
                        self.llvm,
                        if moved {
                            "hew_vec_push_owned_move"
                        } else {
                            "hew_vec_push_owned"
                        },
                        self.ctx
                            .void_type()
                            .fn_type(&[pointer.into(), pointer.into()], false),
                    )?;
                    self.runtime_call_void(
                        function,
                        &[vector.into(), self.slots[source(1)?.0 as usize].into()],
                        "vector.push",
                    )?;
                    if moved {
                        self.clear_owned(source(1)?)?;
                    }
                } else {
                    let function =
                        external_unary_ptr(self.ctx, self.llvm, "hew_vec_clear_release")?;
                    let cursor = self
                        .runtime_call_value(function, &[vector.into()], "vector.clear")?
                        .into_pointer_value();
                    self.clear_owned(receiver)?;
                    self.store(result, vector.into())?;
                    release::drain(&self.value_emitter(), self.frame.as_ref(), cursor)?;
                    return self.emit_result_edge(Some(result), normal);
                }
                self.clear_owned(receiver)?;
                self.store(result, vector.into())?;
            }
            PhysicalVectorOp::Swap => {
                let left = self.load(source(1)?, "vector.swap.left")?.into_int_value();
                let right = self.load(source(2)?, "vector.swap.right")?.into_int_value();
                self.emit_vector_index_guard(vector, left, failure()?)?;
                self.emit_vector_index_guard(vector, right, failure()?)?;
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_vec_swap",
                    self.ctx
                        .void_type()
                        .fn_type(&[pointer.into(), i64_ty.into(), i64_ty.into()], false),
                )?;
                self.runtime_call_void(
                    function,
                    &[vector.into(), left.into(), right.into()],
                    "vector.swap",
                )?;
                self.clear_owned(receiver)?;
                self.store(result, vector.into())?;
            }
            PhysicalVectorOp::Set => {
                let index = self.load(source(1)?, "vector.set.index")?.into_int_value();
                self.emit_vector_index_guard(vector, index, failure()?)?;
                let moved = matches!(transfers.get(2), Some(ArgumentTransfer::Move(_)));
                if scalar {
                    self.make_vector_unique(vector)?;
                    let element = llvm_type(self.ctx, &element_layout.repr)?;
                    let slot = self.vector_element_slot(vector, index, element)?;
                    let replacement = self.load(source(2)?, "vector.replacement")?;
                    self.builder
                        .build_store(slot, replacement)
                        .llvm_ctx("replace scalar vector element")?;
                    if moved {
                        self.clear_owned(source(2)?)?;
                    }
                    self.clear_owned(receiver)?;
                    self.store(result, vector.into())?;
                    return self.emit_result_edge(Some(result), normal);
                }
                let function = get_or_declare_external(
                    self.llvm,
                    if moved {
                        "hew_vec_set_owned_move_release"
                    } else {
                        "hew_vec_set_owned_release"
                    },
                    pointer.fn_type(&[pointer.into(), i64_ty.into(), pointer.into()], false),
                )?;
                // The set releases the element it displaces, so a `close` that
                // fails inside it reaches this frame's fault record.
                let replacement = self.slots[source(2)?.0 as usize];
                let cursor = self
                    .runtime_call_value(
                        function,
                        &[vector.into(), index.into(), replacement.into()],
                        "vector.set",
                    )?
                    .into_pointer_value();
                if moved {
                    self.clear_owned(source(2)?)?;
                }
                self.clear_owned(receiver)?;
                self.store(result, vector.into())?;
                release::drain(&self.value_emitter(), self.frame.as_ref(), cursor)?;
            }
            PhysicalVectorOp::Index
            | PhysicalVectorOp::IndexBorrow
            | PhysicalVectorOp::Get { .. }
            | PhysicalVectorOp::GetBorrow { .. } => {
                let element_ty = llvm_type(self.ctx, &element_layout.repr)?;
                let output = if matches!(
                    operation,
                    PhysicalVectorOp::Index | PhysicalVectorOp::IndexBorrow
                ) {
                    self.slots[result.0 as usize]
                } else {
                    values.entry_scratch(element_ty, "vector.get.element")?
                };
                // A borrowed read aliases the slot the vector still owns; the
                // owning read hands back a fresh owner.
                let index = self.load(source(1)?, "vector.index")?.into_int_value();
                let found = if scalar {
                    self.builder
                        .build_int_compare(
                            IntPredicate::ULT,
                            index,
                            self.vector_length(vector)?,
                            "vector.found",
                        )
                        .llvm_ctx("check scalar vector read bounds")?
                } else {
                    let function = get_or_declare_external(
                        self.llvm,
                        if matches!(
                            operation,
                            PhysicalVectorOp::GetBorrow { .. } | PhysicalVectorOp::IndexBorrow
                        ) {
                            "hew_vec_borrow_owned"
                        } else {
                            "hew_vec_get_clone"
                        },
                        self.ctx
                            .bool_type()
                            .fn_type(&[pointer.into(), i64_ty.into(), pointer.into()], false),
                    )?;
                    self.runtime_call_value(
                        function,
                        &[vector.into(), index.into(), output.into()],
                        "vector.found",
                    )?
                    .into_int_value()
                };
                let present = self
                    .ctx
                    .append_basic_block(self.value, "vector.element.present");
                let absent = self
                    .ctx
                    .append_basic_block(self.value, "vector.element.absent");
                self.builder
                    .build_conditional_branch(found, present, absent)
                    .llvm_ctx("select vector read outcome")?;
                let optional = match operation {
                    PhysicalVectorOp::Get { result: option }
                    | PhysicalVectorOp::GetBorrow { result: option } => Some(option),
                    _ => None,
                };
                self.builder.position_at_end(absent);
                if let Some(option) = optional {
                    self.write_variant_value(self.slots[result.0 as usize], 1, &[], option)?;
                    self.emit_result_edge(Some(result), normal)?;
                } else {
                    self.emit_edge(failure()?)?;
                }
                self.builder.position_at_end(present);
                if scalar {
                    let slot = self.vector_element_slot(vector, index, element_ty)?;
                    let element = self
                        .builder
                        .build_load(element_ty, slot, "vector.scalar")
                        .llvm_ctx("read scalar vector element")?;
                    self.builder
                        .build_store(output, element)
                        .llvm_ctx("initialize scalar vector read")?;
                }
                if let Some(option) = optional {
                    let element = self
                        .builder
                        .build_load(element_ty, output, "vector.get.value")
                        .llvm_ctx("load vector element")?;
                    self.write_variant_value(self.slots[result.0 as usize], 0, &[element], option)?;
                }
            }
            PhysicalVectorOp::Append => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_vec_append",
                    self.ctx
                        .void_type()
                        .fn_type(&[pointer.into(), pointer.into()], false),
                )?;
                let source_vector = self
                    .load(source(1)?, "vector.append.source")?
                    .into_pointer_value();
                self.runtime_call_void(
                    function,
                    &[vector.into(), source_vector.into()],
                    "vector.append",
                )?;
                self.clear_owned(receiver)?;
                self.store(result, vector.into())?;
            }
            PhysicalVectorOp::Join => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_vec_join_str",
                    pointer.fn_type(&[pointer.into(), pointer.into()], false),
                )?;
                let separator = self
                    .load(source(1)?, "vector.join.separator")?
                    .into_pointer_value();
                let joined = self.runtime_call_value(
                    function,
                    &[vector.into(), separator.into()],
                    "vector.join",
                )?;
                self.store(result, joined)?;
            }
            PhysicalVectorOp::Slice | PhysicalVectorOp::SliceFrom => {
                let length_fn = get_or_declare_external(
                    self.llvm,
                    "hew_vec_len",
                    i64_ty.fn_type(&[pointer.into()], false),
                )?;
                let length = self
                    .runtime_call_value(length_fn, &[vector.into()], "vector.slice.length")?
                    .into_int_value();
                let start = self
                    .load(source(1)?, "vector.slice.start")?
                    .into_int_value();
                let end = if operation == PhysicalVectorOp::Slice {
                    self.load(source(2)?, "vector.slice.end")?.into_int_value()
                } else {
                    length
                };
                let start_negative = self
                    .builder
                    .build_int_compare(
                        IntPredicate::SLT,
                        start,
                        i64_ty.const_zero(),
                        "vector.slice.start.negative",
                    )
                    .llvm_ctx("guard negative vector slice start")?;
                let end_negative = self
                    .builder
                    .build_int_compare(
                        IntPredicate::SLT,
                        end,
                        i64_ty.const_zero(),
                        "vector.slice.end.negative",
                    )
                    .llvm_ctx("guard negative vector slice end")?;
                let inverted = self
                    .builder
                    .build_int_compare(IntPredicate::SGT, start, end, "vector.slice.inverted")
                    .llvm_ctx("guard inverted vector slice range")?;
                let past_end = self
                    .builder
                    .build_int_compare(IntPredicate::SGT, end, length, "vector.slice.past.end")
                    .llvm_ctx("guard vector slice upper bound")?;
                let out_of_bounds = self
                    .builder
                    .build_or(start_negative, end_negative, "vector.slice.bounds.a")
                    .and_then(|a| self.builder.build_or(a, inverted, "vector.slice.bounds.b"))
                    .and_then(|b| {
                        self.builder
                            .build_or(b, past_end, "vector.slice.bounds.condition")
                    })
                    .llvm_ctx("combine vector slice guards")?;
                let safe = self.ctx.append_basic_block(self.value, "vector.slice.safe");
                let failed = self
                    .ctx
                    .append_basic_block(self.value, "vector.slice.failure");
                self.builder
                    .build_conditional_branch(out_of_bounds, failed, safe)
                    .llvm_ctx("branch around fallible vector slice call")?;
                self.builder.position_at_end(failed);
                self.emit_edge(failure()?)?;
                self.builder.position_at_end(safe);
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_vec_slice_range_owned",
                    pointer.fn_type(&[pointer.into(), i64_ty.into(), i64_ty.into()], false),
                )?;
                let sliced = self.runtime_call_value(
                    function,
                    &[vector.into(), start.into(), end.into()],
                    "vector.slice",
                )?;
                self.store(result, sliced)?;
            }
            PhysicalVectorOp::Pop { result: tuple }
            | PhysicalVectorOp::Remove { result: tuple }
            | PhysicalVectorOp::TakeFirst { result: tuple } => {
                values.aggregate_glue(tuple)?;
                let layout = self.module.target.layout(&glue.element.ty).ok_or_else(|| {
                    CodegenError::FailClosed("vector element lacks its target layout".into())
                })?;
                let element_ty = llvm_type(self.ctx, &layout.repr)?;
                let output = values.entry_scratch(element_ty, "vector.pop.element")?;
                // Indexed removal and iteration share the same owning take;
                // pop selects the final element through its existing entry.
                let index = match operation {
                    PhysicalVectorOp::TakeFirst { .. } => Some(i64_ty.const_zero()),
                    PhysicalVectorOp::Remove { .. } => {
                        let index = self
                            .load(source(1)?, "vector.remove.index")?
                            .into_int_value();
                        self.emit_vector_index_guard(vector, index, failure()?)?;
                        Some(index)
                    }
                    _ => None,
                };
                let status = if let Some(index) = index {
                    let function = get_or_declare_external(
                        self.llvm,
                        "hew_vec_remove_at_owned",
                        self.ctx
                            .i32_type()
                            .fn_type(&[pointer.into(), i64_ty.into(), pointer.into()], false),
                    )?;
                    self.runtime_call_value(
                        function,
                        &[vector.into(), index.into(), output.into()],
                        "vector.take.status",
                    )?
                    .into_int_value()
                } else {
                    let function = get_or_declare_external(
                        self.llvm,
                        "hew_vec_pop_owned",
                        self.ctx
                            .i32_type()
                            .fn_type(&[pointer.into(), pointer.into()], false),
                    )?;
                    self.runtime_call_value(
                        function,
                        &[vector.into(), output.into()],
                        "vector.pop.status",
                    )?
                    .into_int_value()
                };
                let found = self
                    .builder
                    .build_int_compare(
                        IntPredicate::NE,
                        status,
                        self.ctx.i32_type().const_zero(),
                        "vector.pop.found",
                    )
                    .llvm_ctx("test vector pop outcome")?;
                let present = self
                    .ctx
                    .append_basic_block(self.value, "vector.pop.present");
                let absent = self.ctx.append_basic_block(self.value, "vector.pop.empty");
                self.builder
                    .build_conditional_branch(found, present, absent)
                    .llvm_ctx("select vector pop outcome")?;
                self.builder.position_at_end(absent);
                self.emit_edge(failure()?)?;
                self.builder.position_at_end(present);
                let element = self
                    .builder
                    .build_load(element_ty, output, "vector.pop.value")
                    .llvm_ctx("load transferred vector element")?;
                let tuple_ty =
                    llvm_type(self.ctx, &self.storage(result)?.layout.repr)?.into_struct_type();
                let pair = self
                    .builder
                    .build_insert_value(tuple_ty.const_zero(), vector, 0, "vector.pop.receiver")
                    .llvm_ctx("construct updated vector result")?
                    .into_struct_value();
                let pair = self
                    .builder
                    .build_insert_value(pair, element, 1, "vector.pop.result")
                    .llvm_ctx("construct removed element result")?
                    .into_struct_value();
                self.clear_owned(receiver)?;
                self.store(result, pair.into())?;
            }
        }
        self.emit_result_edge(Some(result), normal)
    }

    pub(super) fn descriptor_pointer(&self, symbol: &str) -> CodegenResult<PointerValue<'ctx>> {
        self.llvm
            .get_global(symbol)
            .map(|global| global.as_pointer_value())
            .ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "collection descriptor `{symbol}` was not emitted"
                ))
            })
    }

    fn store_receiver_pair(
        &self,
        result: StorageId,
        receiver: StorageId,
        value: BasicValueEnum<'ctx>,
    ) -> CodegenResult<()> {
        let pair_ty = llvm_type(self.ctx, &self.storage(result)?.layout.repr)?.into_struct_type();
        let pair = self
            .builder
            .build_insert_value(
                pair_ty.const_zero(),
                self.load(receiver, "collection.updated")?,
                0,
                "collection.pair.receiver",
            )
            .llvm_ctx("construct updated collection result")?
            .into_struct_value();
        let pair = self
            .builder
            .build_insert_value(pair, value, 1, "collection.pair.value")
            .llvm_ctx("construct collection value result")?
            .into_struct_value();
        self.clear_owned(receiver)?;
        self.store(result, pair.into())
    }

    #[expect(
        clippy::too_many_lines,
        reason = "map actions execute their exact runtime and storage contracts"
    )]
    pub(super) fn emit_map_call(
        &self,
        action: (PhysicalMapOp, PhysicalMapId),
        transfers: &[ArgumentTransfer],
        result: StorageId,
        normal: &PhysicalEdge,
        failure: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        let (operation, glue) = action;
        let source = |index: usize| {
            transfers.get(index).map(argument_source).ok_or_else(|| {
                CodegenError::FailClosed(format!("map action lacks argument {index}"))
            })
        };
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        if operation == PhysicalMapOp::New {
            let key = self.descriptor_pointer(&map_key_descriptor_symbol(glue))?;
            let value = self.descriptor_pointer(&map_value_descriptor_symbol(glue))?;
            let function = get_or_declare_external(
                self.llvm,
                "hew_hashmap_new_with_layout",
                pointer.fn_type(&[pointer.into(), pointer.into()], false),
            )?;
            let map = self.runtime_call_value(function, &[key.into(), value.into()], "map.new")?;
            self.store(result, map)?;
            return self.emit_result_edge(Some(result), normal);
        }
        let receiver = source(0)?;
        let map = self.load(receiver, "map.receiver")?;
        match operation {
            PhysicalMapOp::New => unreachable!("constructor already emitted"),
            PhysicalMapOp::Get { .. }
            | PhysicalMapOp::GetBorrow { .. }
            | PhysicalMapOp::Index
            | PhysicalMapOp::Remove { .. } => {
                return self.emit_map_lookup(
                    action,
                    (receiver, source(1)?),
                    result,
                    normal,
                    failure,
                );
            }
            PhysicalMapOp::Len => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_hashmap_len_layout",
                    self.ctx.i64_type().fn_type(&[pointer.into()], false),
                )?;
                let len = self.runtime_call_value(function, &[map.into()], "map.length")?;
                self.store(result, len)?;
            }
            PhysicalMapOp::ContainsKey => {
                let (contains, _) = self.emit_collection_probe(
                    CollectionProbe {
                        key: &self.module.map_glue[glue.0 as usize].key.ty,
                        begin: "hew_hashmap_probe_begin",
                        receiver: map,
                        input: self.slots[source(1)?.0 as usize],
                        inserting: false,
                        commit: "hew_hashmap_probe_contains",
                        commit_args: &[],
                        releases: false,
                    },
                    failure,
                )?;
                self.store(result, contains.into())?;
            }
            PhysicalMapOp::Insert | PhysicalMapOp::Clear => {
                let cursor = if operation == PhysicalMapOp::Insert {
                    // The adopted value transfers into the slot; the key is
                    // cloned in either entry point.
                    let moved = matches!(transfers.get(2), Some(ArgumentTransfer::Move(_)));
                    let (_, cursor) = self.emit_collection_probe(
                        CollectionProbe {
                            key: &self.module.map_glue[glue.0 as usize].key.ty,
                            begin: "hew_hashmap_probe_begin",
                            receiver: map,
                            input: self.slots[source(1)?.0 as usize],
                            inserting: true,
                            commit: if moved {
                                "hew_hashmap_probe_insert_take"
                            } else {
                                "hew_hashmap_probe_insert_clone"
                            },
                            commit_args: &[self.slots[source(2)?.0 as usize].into()],
                            releases: true,
                        },
                        failure,
                    )?;
                    if moved {
                        self.clear_owned(source(2)?)?;
                    }
                    cursor.expect("map insertion detaches displaced owners")
                } else {
                    let function =
                        external_unary_ptr(self.ctx, self.llvm, "hew_hashmap_clear_release")?;
                    self.runtime_call_value(function, &[map.into()], "map.clear")?
                        .into_pointer_value()
                };
                self.clear_owned(receiver)?;
                self.store(result, map)?;
                release::drain(&self.value_emitter(), self.frame.as_ref(), cursor)?;
            }
            PhysicalMapOp::Keys | PhysicalMapOp::Values => {
                let symbol = if operation == PhysicalMapOp::Keys {
                    "hew_hashmap_keys_layout"
                } else {
                    "hew_hashmap_values_layout"
                };
                let function = external_unary_ptr(self.ctx, self.llvm, symbol)?;
                let vector = self.runtime_call_value(function, &[map.into()], "map.projection")?;
                self.store(result, vector)?;
            }
            PhysicalMapOp::Entries { result: vector } => {
                let values = self.value_emitter();
                let recipe = &values.vector_glue(vector)?.element;
                let layout = self.module.target.layout(&recipe.ty).ok_or_else(|| {
                    CodegenError::FailClosed("map entry has no target layout".into())
                })?;
                let pair_ty = llvm_type(self.ctx, &layout.repr)?.into_struct_type();
                let target = TargetData::create(&self.module.target.data_layout);
                let offset = target.offset_of_element(&pair_ty, 1).ok_or_else(|| {
                    CodegenError::FailClosed("map entry has no value offset".into())
                })?;
                let descriptor = self.descriptor_pointer(&vector_descriptor_symbol(vector))?;
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_hashmap_entries_layout",
                    pointer.fn_type(
                        &[pointer.into(), pointer.into(), self.ctx.i64_type().into()],
                        false,
                    ),
                )?;
                let entries = self.runtime_call_value(
                    function,
                    &[
                        map.into(),
                        descriptor.into(),
                        self.ctx.i64_type().const_int(offset, false).into(),
                    ],
                    "map.entries",
                )?;
                self.store(result, entries)?;
            }
        }
        self.emit_result_edge(Some(result), normal)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "lookup and removal share presence-sensitive output initialization"
    )]
    fn emit_map_lookup(
        &self,
        action: (PhysicalMapOp, PhysicalMapId),
        inputs: (StorageId, StorageId),
        result: StorageId,
        normal: &PhysicalEdge,
        failure: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        let (operation, id) = action;
        let (receiver, key) = inputs;
        let glue = self
            .module
            .map_glue
            .get(id.0 as usize)
            .filter(|glue| glue.id == id)
            .ok_or_else(|| CodegenError::FailClosed("unknown map descriptor".into()))?;
        let layout = self
            .module
            .target
            .layout(&glue.value.ty)
            .ok_or_else(|| CodegenError::FailClosed("map value has no target layout".into()))?;
        let value_ty = llvm_type(self.ctx, &layout.repr)?;
        let values = self.value_emitter();
        let output = if operation == PhysicalMapOp::Index {
            self.slots[result.0 as usize]
        } else {
            values.entry_scratch(value_ty, "map.lookup.value")?
        };
        let option = match operation {
            PhysicalMapOp::Get { result: option }
            | PhysicalMapOp::GetBorrow { result: option }
            | PhysicalMapOp::Remove { value: option, .. } => Some(option),
            PhysicalMapOp::Index => None,
            _ => return Err(CodegenError::FailClosed("non-lookup map action".into())),
        };
        let option_slot = if let Some(option) = option {
            let layout = &values
                .variant_layout(&values.variant_glue(option)?.ty)?
                .object;
            Some(values.entry_scratch(llvm_type(self.ctx, &layout.repr)?, "map.optional.value")?)
        } else {
            None
        };
        // A borrowed read aliases the value the map still owns; the owning
        // read hands back a fresh owner and the removal moves one out.
        let symbol = match operation {
            PhysicalMapOp::Remove { .. } => "hew_hashmap_probe_remove_take",
            PhysicalMapOp::GetBorrow { .. } => "hew_hashmap_probe_get_borrow",
            _ => "hew_hashmap_probe_get_clone",
        };
        let (found, cursor) = self.emit_collection_probe(
            CollectionProbe {
                key: &glue.key.ty,
                begin: "hew_hashmap_probe_begin",
                receiver: self.load(receiver, "map.lookup.receiver")?,
                input: self.slots[key.0 as usize],
                inserting: false,
                commit: symbol,
                commit_args: &[output.into()],
                releases: matches!(operation, PhysicalMapOp::Remove { .. }),
            },
            failure,
        )?;
        let found = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                found,
                self.ctx.i8_type().const_zero(),
                "map.lookup.found",
            )
            .llvm_ctx("normalize map lookup presence")?;
        let present = self
            .ctx
            .append_basic_block(self.value, "map.lookup.present");
        let absent = self.ctx.append_basic_block(self.value, "map.lookup.absent");
        let complete = self
            .ctx
            .append_basic_block(self.value, "map.lookup.complete");
        self.builder
            .build_conditional_branch(found, present, absent)
            .llvm_ctx("select map lookup outcome")?;
        self.builder.position_at_end(absent);
        if let (Some(option), Some(slot)) = (option, option_slot) {
            self.write_variant_value(slot, 1, &[], option)?;
            self.builder
                .build_unconditional_branch(complete)
                .llvm_ctx("finish absent map value")?;
        } else {
            self.initialize_active_fault(HEW_TRAP_INDEX_OUT_OF_BOUNDS)?;
            self.emit_edge(failure.ok_or_else(|| {
                CodegenError::FailClosed("map index lacks its failure cleanup".into())
            })?)?;
        }
        self.builder.position_at_end(present);
        if let (Some(option), Some(slot)) = (option, option_slot) {
            let value = self
                .builder
                .build_load(value_ty, output, "map.lookup.owner")
                .llvm_ctx("load map value")?;
            self.write_variant_value(slot, 0, &[value], option)?;
        }
        self.builder
            .build_unconditional_branch(complete)
            .llvm_ctx("finish present map value")?;
        self.builder.position_at_end(complete);
        if let (Some(option), Some(slot)) = (option, option_slot) {
            let layout = &values
                .variant_layout(&values.variant_glue(option)?.ty)?
                .object;
            let value = self
                .builder
                .build_load(
                    llvm_type(self.ctx, &layout.repr)?,
                    slot,
                    "map.optional.owner",
                )
                .llvm_ctx("load initialized optional map value")?;
            if matches!(operation, PhysicalMapOp::Remove { .. }) {
                self.store_receiver_pair(result, receiver, value)?;
            } else {
                self.store(result, value)?;
            }
        }
        if let Some(cursor) = cursor {
            release::drain(&self.value_emitter(), self.frame.as_ref(), cursor)?;
        }
        self.emit_result_edge(Some(result), normal)
    }

    pub(super) fn emit_set_call(
        &self,
        action: (PhysicalSetOp, PhysicalSetId),
        transfers: &[ArgumentTransfer],
        result: StorageId,
        normal: &PhysicalEdge,
        failure: Option<&PhysicalEdge>,
    ) -> CodegenResult<()> {
        let (operation, glue) = action;
        let source = |index: usize| {
            transfers.get(index).map(argument_source).ok_or_else(|| {
                CodegenError::FailClosed(format!("set action lacks argument {index}"))
            })
        };
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        if operation == PhysicalSetOp::New {
            let descriptor = self.descriptor_pointer(&set_key_descriptor_symbol(glue))?;
            let function = external_unary_ptr(self.ctx, self.llvm, "hew_hashset_new_with_layout")?;
            let set = self.runtime_call_value(function, &[descriptor.into()], "set.new")?;
            self.store(result, set)?;
            return self.emit_result_edge(Some(result), normal);
        }
        let receiver = source(0)?;
        let set = self.load(receiver, "set.receiver")?;
        match operation {
            PhysicalSetOp::New => unreachable!("constructor already emitted"),
            PhysicalSetOp::Len => {
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_hashset_len_layout",
                    self.ctx.i64_type().fn_type(&[pointer.into()], false),
                )?;
                let len = self.runtime_call_value(function, &[set.into()], "set.length")?;
                self.store(result, len)?;
            }
            PhysicalSetOp::Contains
            | PhysicalSetOp::Insert { .. }
            | PhysicalSetOp::Remove { .. } => {
                // An adopted element transfers into the set; a borrowed one is
                // cloned. The take entry consumes the caller's element on the
                // duplicate path too, so nothing is left for the caller to
                // release either way.
                let moved = matches!(transfers.get(1), Some(ArgumentTransfer::Move(_)));
                let symbol = match operation {
                    PhysicalSetOp::Contains => "hew_hashmap_probe_contains",
                    PhysicalSetOp::Insert { .. } if moved => "hew_hashset_probe_insert_take",
                    PhysicalSetOp::Insert { .. } => "hew_hashset_probe_insert_clone",
                    PhysicalSetOp::Remove { .. } => "hew_hashset_probe_remove",
                    _ => unreachable!("matched membership operation"),
                };
                let (present, cursor) = self.emit_collection_probe(
                    CollectionProbe {
                        key: &self.module.set_glue[glue.0 as usize].element.ty,
                        begin: "hew_hashset_probe_begin",
                        receiver: set,
                        input: self.slots[source(1)?.0 as usize],
                        inserting: matches!(operation, PhysicalSetOp::Insert { .. }),
                        commit: symbol,
                        commit_args: &[],
                        releases: operation != PhysicalSetOp::Contains,
                    },
                    failure,
                )?;
                if matches!(operation, PhysicalSetOp::Insert { .. }) && moved {
                    self.clear_owned(source(1)?)?;
                }
                if operation == PhysicalSetOp::Contains {
                    self.store(result, present.into())?;
                } else {
                    self.store_receiver_pair(result, receiver, present.into())?;
                }
                if let Some(cursor) = cursor {
                    release::drain(&self.value_emitter(), self.frame.as_ref(), cursor)?;
                }
            }
            PhysicalSetOp::Clear => {
                let function =
                    external_unary_ptr(self.ctx, self.llvm, "hew_hashset_clear_release")?;
                let cursor = self
                    .runtime_call_value(function, &[set.into()], "set.clear")?
                    .into_pointer_value();
                self.clear_owned(receiver)?;
                self.store(result, set)?;
                release::drain(&self.value_emitter(), self.frame.as_ref(), cursor)?;
            }
            PhysicalSetOp::Elements => {
                let function =
                    external_unary_ptr(self.ctx, self.llvm, "hew_hashset_to_vec_layout")?;
                let vector = self.runtime_call_value(function, &[set.into()], "set.elements")?;
                self.store(result, vector)?;
            }
        }
        self.emit_result_edge(Some(result), normal)
    }
}
