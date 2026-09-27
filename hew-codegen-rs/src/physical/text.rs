//! UTF-8, bytes and string operations.

use super::*;

impl<'a, 'ctx> FunctionEmitter<'a, 'ctx> {
    pub(super) fn emit_utf8_decode(
        &self,
        bytes: StorageId,
        result: StorageId,
        result_glue: PhysicalVariantId,
        error_glue: PhysicalAggregateId,
        option_glue: PhysicalVariantId,
    ) -> CodegenResult<()> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let target = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let value_out = self
            .value_emitter()
            .entry_scratch(ptr.into(), "utf8.value")?;
        let valid_out = self
            .value_emitter()
            .entry_scratch(size_ty.into(), "utf8.valid.up.to")?;
        let length_out = self
            .value_emitter()
            .entry_scratch(size_ty.into(), "utf8.error.length")?;
        let function = get_or_declare_external(
            self.llvm,
            "hew_bytes_decode_utf8",
            self.ctx
                .i8_type()
                .fn_type(&[ptr.into(), ptr.into(), ptr.into(), ptr.into()], false),
        )?;
        let status = self
            .runtime_call_value(
                function,
                &[
                    self.slots[bytes.0 as usize].into(),
                    value_out.into(),
                    valid_out.into(),
                    length_out.into(),
                ],
                "utf8.status",
            )?
            .into_int_value();
        let success = self.ctx.append_basic_block(self.value, "utf8.success");
        let failure = self.ctx.append_basic_block(self.value, "utf8.error");
        let invalid = self
            .ctx
            .append_basic_block(self.value, "utf8.invalid.status");
        let complete = self.ctx.append_basic_block(self.value, "utf8.complete");
        self.builder
            .build_switch(
                status,
                invalid,
                &[
                    (self.ctx.i8_type().const_zero(), success),
                    (self.ctx.i8_type().const_int(1, false), failure),
                ],
            )
            .llvm_ctx("select UTF-8 value outcome")?;

        self.builder.position_at_end(success);
        let value = self
            .builder
            .build_load(ptr, value_out, "utf8.string")
            .llvm_ctx("load successful UTF-8 string")?;
        self.write_variant_value(self.slots[result.0 as usize], 0, &[value], result_glue)?;
        self.builder
            .build_unconditional_branch(complete)
            .llvm_ctx("finish UTF-8 success")?;

        self.builder.position_at_end(failure);
        let valid = self
            .builder
            .build_load(size_ty, valid_out, "utf8.valid")
            .llvm_ctx("load UTF-8 valid prefix")?
            .into_int_value();
        let length = self
            .builder
            .build_load(size_ty, length_out, "utf8.length")
            .llvm_ctx("load UTF-8 invalid sequence length")?
            .into_int_value();
        let valid = self
            .builder
            .build_int_z_extend_or_bit_cast(valid, self.ctx.i64_type(), "utf8.valid.i64")
            .llvm_ctx("widen UTF-8 byte position")?;
        let length = self
            .builder
            .build_int_z_extend_or_bit_cast(length, self.ctx.i64_type(), "utf8.length.i64")
            .llvm_ctx("widen UTF-8 error length")?;
        let option_ty = llvm_type(
            self.ctx,
            &self
                .value_emitter()
                .variant_layout(&self.value_emitter().variant_glue(option_glue)?.ty)?
                .object
                .repr,
        )?;
        let option = self
            .value_emitter()
            .entry_scratch(option_ty, "utf8.optional.length")?;
        let some = self.ctx.append_basic_block(self.value, "utf8.length.some");
        let none = self.ctx.append_basic_block(self.value, "utf8.length.none");
        let error_ready = self.ctx.append_basic_block(self.value, "utf8.error.ready");
        let incomplete = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                length,
                self.ctx.i64_type().const_zero(),
                "utf8.incomplete",
            )
            .llvm_ctx("classify incomplete UTF-8")?;
        self.builder
            .build_conditional_branch(incomplete, none, some)
            .llvm_ctx("select UTF-8 error length")?;
        self.builder.position_at_end(some);
        self.write_variant_value(option, 0, &[length.into()], option_glue)?;
        self.builder
            .build_unconditional_branch(error_ready)
            .llvm_ctx("finish known error length")?;
        self.builder.position_at_end(none);
        self.write_variant_value(option, 1, &[], option_glue)?;
        self.builder
            .build_unconditional_branch(error_ready)
            .llvm_ctx("finish incomplete error length")?;
        self.builder.position_at_end(error_ready);
        let option = self
            .builder
            .build_load(option_ty, option, "utf8.error.option")
            .llvm_ctx("load initialized UTF-8 error length")?;
        let error_ty = &self.value_emitter().aggregate_glue(error_glue)?.ty;
        let error_layout =
            self.module.target.layout(error_ty).ok_or_else(|| {
                CodegenError::FailClosed("UTF-8 error has no physical layout".into())
            })?;
        let record_ty = llvm_type(self.ctx, &error_layout.repr)?.into_struct_type();
        let record = self
            .builder
            .build_insert_value(record_ty.get_undef(), valid, 0, "utf8.error.position")
            .llvm_ctx("construct UTF-8 error position")?
            .into_struct_value();
        let record = self
            .builder
            .build_insert_value(record, option, 1, "utf8.error.record")
            .llvm_ctx("construct UTF-8 error length")?
            .into_struct_value();
        self.write_variant_value(
            self.slots[result.0 as usize],
            1,
            &[record.into()],
            result_glue,
        )?;
        self.builder
            .build_unconditional_branch(complete)
            .llvm_ctx("finish UTF-8 error")?;
        self.builder.position_at_end(invalid);
        self.value_emitter().emit_invalid_variant_tag()?;
        self.builder.position_at_end(complete);
        Ok(())
    }

    pub(super) fn emit_bytes_index(
        &self,
        bytes: StorageId,
        index: StorageId,
        result: StorageId,
        normal: &PhysicalEdge,
        failure: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let value = self.load(bytes, "bytes.index.value")?.into_struct_value();
        let pointer = self
            .builder
            .build_extract_value(value, 0, "bytes.index.pointer")
            .llvm_ctx("extract bytes index pointer")?
            .into_pointer_value();
        let offset = self
            .builder
            .build_extract_value(value, 1, "bytes.index.offset")
            .llvm_ctx("extract bytes index offset")?
            .into_int_value();
        let len = self
            .builder
            .build_extract_value(value, 2, "bytes.index.length")
            .llvm_ctx("extract bytes index length")?
            .into_int_value();
        let index = self.load(index, "bytes.index.index")?.into_int_value();
        let len64 = self
            .builder
            .build_int_z_extend(len, self.ctx.i64_type(), "bytes.index.length.i64")
            .llvm_ctx("widen bytes length")?;
        let offset64 = self
            .builder
            .build_int_z_extend(offset, self.ctx.i64_type(), "bytes.index.offset.i64")
            .llvm_ctx("widen bytes offset")?;
        let byte_offset = self
            .builder
            .build_int_add(offset64, index, "bytes.index.byte.offset")
            .llvm_ctx("calculate bytes index offset")?;
        let negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                index,
                self.ctx.i64_type().const_zero(),
                "bytes.index.negative",
            )
            .llvm_ctx("guard negative bytes index")?;
        let past_end = self
            .builder
            .build_int_compare(IntPredicate::SGE, index, len64, "bytes.index.past.end")
            .llvm_ctx("guard bytes index upper bound")?;
        let null = self
            .builder
            .build_is_null(pointer, "bytes.index.null")
            .llvm_ctx("guard null bytes index pointer")?;
        let offset_overflow = self
            .builder
            .build_int_compare(
                IntPredicate::UGT,
                byte_offset,
                self.ctx.i64_type().const_int(u64::from(u32::MAX), false),
                "bytes.index.offset.overflow",
            )
            .llvm_ctx("guard bytes index offset overflow")?;
        let out_of_bounds = self
            .builder
            .build_or(negative, past_end, "bytes.index.bounds")
            .and_then(|bounds| self.builder.build_or(bounds, null, "bytes.index.invalid"))
            .and_then(|invalid| {
                self.builder
                    .build_or(invalid, offset_overflow, "bytes.index.failure.condition")
            })
            .llvm_ctx("combine bytes index guards")?;
        let safe = self.ctx.append_basic_block(self.value, "bytes.index.safe");
        let failed = self
            .ctx
            .append_basic_block(self.value, "bytes.index.failure");
        self.builder
            .build_conditional_branch(out_of_bounds, failed, safe)
            .llvm_ctx("branch around fallible bytes index load")?;

        self.builder.position_at_end(failed);
        self.emit_edge(failure)?;

        self.builder.position_at_end(safe);
        // SAFETY: the physical normal path proves a non-null pointer, an index
        // within the active region, and an offset that is representable by the
        // runtime Bytes layout. Verified owned Bytes storage supplies the
        // allocation-validity invariant for that active region.
        let read_at = unsafe {
            self.builder.build_gep(
                self.ctx.i8_type(),
                pointer,
                &[byte_offset],
                "bytes.index.pointer",
            )
        }
        .llvm_ctx("calculate bytes index pointer")?;
        let indexed = self
            .builder
            .build_load(self.ctx.i8_type(), read_at, "bytes.index.load")
            .llvm_ctx("load indexed byte")?;
        self.store(result, indexed)?;
        self.emit_result_edge(Some(result), normal)
    }

    /// Shrink the receiver in place and build the `(bytes, Option<u8>)` pair.
    ///
    /// `hew_bytes_pop` answers `-1` on an empty buffer; every real byte is in
    /// `0..=255`, so the sentinel is the only `None`.
    pub(super) fn emit_bytes_pop(
        &self,
        receiver: StorageId,
        result: StorageId,
        option: PhysicalVariantId,
        normal: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let function = get_or_declare_external(
            self.llvm,
            "hew_bytes_pop",
            self.ctx
                .i64_type()
                .fn_type(&[self.ctx.ptr_type(AddressSpace::default()).into()], false),
        )?;
        let byte = self
            .runtime_call_value(
                function,
                &[self.slots[receiver.0 as usize].into()],
                "bytes.pop.byte",
            )?
            .into_int_value();
        let destination = self.slots[result.0 as usize];
        let pair_ty = llvm_type(self.ctx, &self.storage(result)?.layout.repr)?.into_struct_type();
        let updated = self.load(receiver, "bytes.pop.receiver")?;
        let owner = self
            .builder
            .build_struct_gep(pair_ty, destination, 0, "bytes.pop.owner")
            .llvm_ctx("address the shrunk bytes receiver")?;
        self.builder
            .build_store(owner, updated)
            .llvm_ctx("write the shrunk bytes receiver")?;
        let optional = self
            .builder
            .build_struct_gep(pair_ty, destination, 1, "bytes.pop.optional")
            .llvm_ctx("address the popped byte")?;
        let found = self
            .builder
            .build_int_compare(
                IntPredicate::SGE,
                byte,
                byte.get_type().const_zero(),
                "bytes.pop.found",
            )
            .llvm_ctx("check bytes pop sentinel")?;
        let some = self.ctx.append_basic_block(self.value, "bytes.pop.some");
        let none = self.ctx.append_basic_block(self.value, "bytes.pop.none");
        self.builder
            .build_conditional_branch(found, some, none)
            .llvm_ctx("select bytes pop outcome")?;
        self.builder.position_at_end(none);
        self.write_variant_value(optional, 1, &[], option)?;
        self.emit_result_edge(Some(result), normal)?;
        self.builder.position_at_end(some);
        let popped = self
            .builder
            .build_int_truncate(byte, self.ctx.i8_type(), "bytes.pop.value")
            .llvm_ctx("narrow the popped byte")?;
        self.write_variant_value(optional, 0, &[popped.into()], option)?;
        self.emit_result_edge(Some(result), normal)
    }

    pub(super) fn emit_bytes_get(
        &self,
        bytes: StorageId,
        index: StorageId,
        result: StorageId,
        option: PhysicalVariantId,
        normal: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let value = self.load(bytes, "bytes.get.value")?.into_struct_value();
        let pointer = self
            .builder
            .build_extract_value(value, 0, "bytes.get.pointer")
            .llvm_ctx("extract bytes get pointer")?
            .into_pointer_value();
        let offset = self
            .builder
            .build_extract_value(value, 1, "bytes.get.offset")
            .llvm_ctx("extract bytes get offset")?
            .into_int_value();
        let len = self
            .builder
            .build_extract_value(value, 2, "bytes.get.length")
            .llvm_ctx("extract bytes get length")?
            .into_int_value();
        let index = self.load(index, "bytes.get.index")?.into_int_value();
        let len64 = self
            .builder
            .build_int_z_extend(len, self.ctx.i64_type(), "bytes.get.length.i64")
            .llvm_ctx("widen bytes get length")?;
        let offset64 = self
            .builder
            .build_int_z_extend(offset, self.ctx.i64_type(), "bytes.get.offset.i64")
            .llvm_ctx("widen bytes get offset")?;
        let byte_offset = self
            .builder
            .build_int_add(offset64, index, "bytes.get.byte.offset")
            .llvm_ctx("calculate bytes get offset")?;
        let negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                index,
                self.ctx.i64_type().const_zero(),
                "bytes.get.negative",
            )
            .llvm_ctx("guard negative bytes get index")?;
        let past_end = self
            .builder
            .build_int_compare(IntPredicate::SGE, index, len64, "bytes.get.past.end")
            .llvm_ctx("guard bytes get upper bound")?;
        let null = self
            .builder
            .build_is_null(pointer, "bytes.get.null")
            .llvm_ctx("guard null bytes get pointer")?;
        let overflow = self
            .builder
            .build_int_compare(
                IntPredicate::UGT,
                byte_offset,
                self.ctx.i64_type().const_int(u64::from(u32::MAX), false),
                "bytes.get.offset.overflow",
            )
            .llvm_ctx("guard bytes get offset overflow")?;
        let invalid = self
            .builder
            .build_or(negative, past_end, "bytes.get.bounds")
            .and_then(|v| self.builder.build_or(v, null, "bytes.get.invalid"))
            .and_then(|v| self.builder.build_or(v, overflow, "bytes.get.failure"))
            .llvm_ctx("combine bytes get guards")?;
        let some = self.ctx.append_basic_block(self.value, "bytes.get.some");
        let none = self.ctx.append_basic_block(self.value, "bytes.get.none");
        self.builder
            .build_conditional_branch(invalid, none, some)
            .llvm_ctx("select bytes get result")?;
        self.builder.position_at_end(none);
        self.write_variant_value(self.slots[result.0 as usize], 1, &[], option)?;
        self.emit_result_edge(Some(result), normal)?;
        self.builder.position_at_end(some);
        let read_at = unsafe {
            self.builder.build_gep(
                self.ctx.i8_type(),
                pointer,
                &[byte_offset],
                "bytes.get.address",
            )
        }
        .llvm_ctx("calculate bytes get address")?;
        let byte = self
            .builder
            .build_load(self.ctx.i8_type(), read_at, "bytes.get.byte")
            .llvm_ctx("load bytes get byte")?;
        self.write_variant_value(self.slots[result.0 as usize], 0, &[byte], option)?;
        self.emit_result_edge(Some(result), normal)
    }

    pub(super) fn emit_bytes_set(
        &self,
        bytes: StorageId,
        index: StorageId,
        byte: StorageId,
        result: StorageId,
        normal: &PhysicalEdge,
        failure: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let value = self.load(bytes, "bytes.set.value")?.into_struct_value();
        let pointer = self
            .builder
            .build_extract_value(value, 0, "bytes.set.pointer")
            .llvm_ctx("extract bytes set pointer")?
            .into_pointer_value();
        let offset = self
            .builder
            .build_extract_value(value, 1, "bytes.set.offset")
            .llvm_ctx("extract bytes set offset")?
            .into_int_value();
        let len = self
            .builder
            .build_extract_value(value, 2, "bytes.set.length")
            .llvm_ctx("extract bytes set length")?
            .into_int_value();
        let index = self.load(index, "bytes.set.index")?.into_int_value();
        let len64 = self
            .builder
            .build_int_z_extend(len, self.ctx.i64_type(), "bytes.set.length.i64")
            .llvm_ctx("widen bytes set length")?;
        let offset64 = self
            .builder
            .build_int_z_extend(offset, self.ctx.i64_type(), "bytes.set.offset.i64")
            .llvm_ctx("widen bytes set offset")?;
        let byte_offset = self
            .builder
            .build_int_add(offset64, index, "bytes.set.byte.offset")
            .llvm_ctx("calculate bytes set offset")?;
        let negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                index,
                self.ctx.i64_type().const_zero(),
                "bytes.set.negative",
            )
            .llvm_ctx("guard negative bytes set index")?;
        let past_end = self
            .builder
            .build_int_compare(IntPredicate::SGE, index, len64, "bytes.set.past.end")
            .llvm_ctx("guard bytes set upper bound")?;
        let null = self
            .builder
            .build_is_null(pointer, "bytes.set.null")
            .llvm_ctx("guard null bytes set pointer")?;
        let overflow = self
            .builder
            .build_int_compare(
                IntPredicate::UGT,
                byte_offset,
                self.ctx.i64_type().const_int(u64::from(u32::MAX), false),
                "bytes.set.offset.overflow",
            )
            .llvm_ctx("guard bytes set offset overflow")?;
        let invalid = self
            .builder
            .build_or(negative, past_end, "bytes.set.bounds")
            .and_then(|v| self.builder.build_or(v, null, "bytes.set.invalid"))
            .and_then(|v| self.builder.build_or(v, overflow, "bytes.set.failure"))
            .llvm_ctx("combine bytes set guards")?;
        let safe = self.ctx.append_basic_block(self.value, "bytes.set.safe");
        let failed = self.ctx.append_basic_block(self.value, "bytes.set.failure");
        self.builder
            .build_conditional_branch(invalid, failed, safe)
            .llvm_ctx("select bytes set outcome")?;
        self.builder.position_at_end(failed);
        self.emit_edge(failure)?;
        self.builder.position_at_end(safe);
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let function = get_or_declare_external(
            self.llvm,
            "hew_bytes_set",
            self.ctx.void_type().fn_type(
                &[
                    ptr.into(),
                    self.ctx.i64_type().into(),
                    self.ctx.i8_type().into(),
                ],
                false,
            ),
        )?;
        self.runtime_call_void(
            function,
            &[
                self.slots[bytes.0 as usize].into(),
                index.into(),
                self.load(byte, "bytes.set.byte")?.into(),
            ],
            "bytes.set",
        )?;
        let updated = self.load(bytes, "bytes.set.result")?;
        self.store(result, updated)?;
        self.emit_result_edge(Some(result), normal)
    }

    /// Call `hew_string_length` on `text` and return its `i64` result.
    fn string_length(
        &self,
        text: StorageId,
        name: &str,
    ) -> CodegenResult<inkwell::values::IntValue<'ctx>> {
        let function = get_or_declare_external(
            self.llvm,
            "hew_string_length",
            self.ctx
                .i64_type()
                .fn_type(&[self.ctx.ptr_type(AddressSpace::default()).into()], false),
        )?;
        let value = self.runtime_call_value(function, &[self.load(text, name)?.into()], name)?;
        Ok(value.into_int_value())
    }

    /// `s[i]` — codepoint index on `string`. MIR proves the bounds check
    /// here (mirrors [`Self::emit_bytes_index`]) so a violation reports
    /// through the canonical `Trap { IndexOutOfBounds }` edge rather than
    /// the runtime's own internal abort path. `hew_string_index` still
    /// carries its own defense-in-depth check, but the codegen guard below
    /// means it can never observe an out-of-range offset in practice.
    pub(super) fn emit_string_index(
        &self,
        text: StorageId,
        index: StorageId,
        result: StorageId,
        normal: &PhysicalEdge,
        failure: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let len = self.string_length(text, "string.index.length")?;
        let index_value = self.load(index, "string.index.index")?.into_int_value();
        let negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                index_value,
                self.ctx.i64_type().const_zero(),
                "string.index.negative",
            )
            .llvm_ctx("guard negative string index")?;
        let past_end = self
            .builder
            .build_int_compare(IntPredicate::SGE, index_value, len, "string.index.past.end")
            .llvm_ctx("guard string index upper bound")?;
        let out_of_bounds = self
            .builder
            .build_or(negative, past_end, "string.index.bounds")
            .llvm_ctx("combine string index guards")?;
        let safe = self.ctx.append_basic_block(self.value, "string.index.safe");
        let failed = self
            .ctx
            .append_basic_block(self.value, "string.index.failure");
        self.builder
            .build_conditional_branch(out_of_bounds, failed, safe)
            .llvm_ctx("branch around fallible string index call")?;

        self.builder.position_at_end(failed);
        self.emit_edge(failure)?;

        self.builder.position_at_end(safe);
        let function = get_or_declare_external(
            self.llvm,
            "hew_string_index",
            self.ctx.i32_type().fn_type(
                &[
                    self.ctx.ptr_type(AddressSpace::default()).into(),
                    self.ctx.i64_type().into(),
                ],
                false,
            ),
        )?;
        let value = self.runtime_call_value(
            function,
            &[
                self.load(text, "string.index.text")?.into(),
                index_value.into(),
            ],
            "string.index",
        )?;
        self.store(result, value)?;
        self.emit_result_edge(Some(result), normal)
    }

    /// `s[a..b]` — codepoint range-slice on `string` (`0 <= start <= end <=
    /// len`, matching [`Self::emit_string_index`]'s MIR-level bounds proof).
    /// `b[a..b]` and its open-ended forms. The bounds guard lives here, like
    /// [`Self::emit_bytes_index`], so a violation reports through the canonical
    /// `Trap { IndexOutOfBounds }` edge instead of the runtime's abort path.
    /// An absent end bound is the receiver's own length.
    pub(super) fn emit_bytes_slice(
        &self,
        bytes: StorageId,
        start: StorageId,
        end: Option<StorageId>,
        result: StorageId,
        normal: &PhysicalEdge,
        failure: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let i64_ty = self.ctx.i64_type();
        let value = self.load(bytes, "bytes.slice.value")?.into_struct_value();
        let len = self
            .builder
            .build_extract_value(value, 2, "bytes.slice.length")
            .llvm_ctx("extract bytes slice length")?
            .into_int_value();
        let len64 = self
            .builder
            .build_int_z_extend(len, i64_ty, "bytes.slice.length.i64")
            .llvm_ctx("widen bytes slice length")?;
        let start_value = self.load(start, "bytes.slice.start")?.into_int_value();
        let end_value = match end {
            Some(end) => self.load(end, "bytes.slice.end")?.into_int_value(),
            None => len64,
        };
        let start_negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                start_value,
                i64_ty.const_zero(),
                "bytes.slice.start.negative",
            )
            .llvm_ctx("guard negative bytes slice start")?;
        let end_negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                end_value,
                i64_ty.const_zero(),
                "bytes.slice.end.negative",
            )
            .llvm_ctx("guard negative bytes slice end")?;
        let inverted = self
            .builder
            .build_int_compare(
                IntPredicate::SGT,
                start_value,
                end_value,
                "bytes.slice.inverted",
            )
            .llvm_ctx("guard inverted bytes slice range")?;
        let past_end = self
            .builder
            .build_int_compare(IntPredicate::SGT, end_value, len64, "bytes.slice.past.end")
            .llvm_ctx("guard bytes slice upper bound")?;
        let out_of_bounds = self
            .builder
            .build_or(start_negative, end_negative, "bytes.slice.bounds.a")
            .and_then(|a| self.builder.build_or(a, inverted, "bytes.slice.bounds.b"))
            .and_then(|b| {
                self.builder
                    .build_or(b, past_end, "bytes.slice.bounds.condition")
            })
            .llvm_ctx("combine bytes slice guards")?;
        let safe = self.ctx.append_basic_block(self.value, "bytes.slice.safe");
        let failed = self
            .ctx
            .append_basic_block(self.value, "bytes.slice.failure");
        self.builder
            .build_conditional_branch(out_of_bounds, failed, safe)
            .llvm_ctx("branch around fallible bytes slice call")?;

        self.builder.position_at_end(failed);
        self.emit_edge(failure)?;

        self.builder.position_at_end(safe);
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let function = get_or_declare_external(
            self.llvm,
            "hew_bytes_slice_owned",
            self.ctx.void_type().fn_type(
                &[ptr.into(), i64_ty.into(), i64_ty.into(), ptr.into()],
                false,
            ),
        )?;
        self.runtime_call_void(
            function,
            &[
                self.slots[bytes.0 as usize].into(),
                start_value.into(),
                end_value.into(),
                self.slots[result.0 as usize].into(),
            ],
            "bytes.slice",
        )?;
        self.emit_result_edge(Some(result), normal)
    }

    pub(super) fn emit_string_slice_codepoints(
        &self,
        text: StorageId,
        start: StorageId,
        end: StorageId,
        result: StorageId,
        normal: &PhysicalEdge,
        failure: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let len = self.string_length(text, "string.slice.length")?;
        let start_value = self.load(start, "string.slice.start")?.into_int_value();
        let end_value = self.load(end, "string.slice.end")?.into_int_value();
        let start_negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                start_value,
                self.ctx.i64_type().const_zero(),
                "string.slice.start.negative",
            )
            .llvm_ctx("guard negative string slice start")?;
        let end_negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                end_value,
                self.ctx.i64_type().const_zero(),
                "string.slice.end.negative",
            )
            .llvm_ctx("guard negative string slice end")?;
        let inverted = self
            .builder
            .build_int_compare(
                IntPredicate::SGT,
                start_value,
                end_value,
                "string.slice.inverted",
            )
            .llvm_ctx("guard inverted string slice range")?;
        let past_end = self
            .builder
            .build_int_compare(IntPredicate::SGT, end_value, len, "string.slice.past.end")
            .llvm_ctx("guard string slice upper bound")?;
        let out_of_bounds = self
            .builder
            .build_or(start_negative, end_negative, "string.slice.bounds.a")
            .and_then(|a| self.builder.build_or(a, inverted, "string.slice.bounds.b"))
            .and_then(|b| {
                self.builder
                    .build_or(b, past_end, "string.slice.bounds.condition")
            })
            .llvm_ctx("combine string slice guards")?;
        let safe = self.ctx.append_basic_block(self.value, "string.slice.safe");
        let failed = self
            .ctx
            .append_basic_block(self.value, "string.slice.failure");
        self.builder
            .build_conditional_branch(out_of_bounds, failed, safe)
            .llvm_ctx("branch around fallible string slice call")?;

        self.builder.position_at_end(failed);
        self.emit_edge(failure)?;

        self.builder.position_at_end(safe);
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let function = get_or_declare_external(
            self.llvm,
            "hew_string_slice_codepoints",
            ptr.fn_type(
                &[
                    ptr.into(),
                    self.ctx.i64_type().into(),
                    self.ctx.i64_type().into(),
                ],
                false,
            ),
        )?;
        let value = self.runtime_call_value(
            function,
            &[
                self.load(text, "string.slice.text")?.into(),
                start_value.into(),
                end_value.into(),
            ],
            "string.slice.codepoints",
        )?;
        self.store(result, value)?;
        self.emit_result_edge(Some(result), normal)
    }

    /// `s[a..]` — open-ended codepoint range-slice on `string` (`0 <= start
    /// <= len`, matching [`Self::emit_string_slice_codepoints`]).
    pub(super) fn emit_string_slice_codepoints_from(
        &self,
        text: StorageId,
        start: StorageId,
        result: StorageId,
        normal: &PhysicalEdge,
        failure: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let len = self.string_length(text, "string.slice.from.length")?;
        let start_value = self
            .load(start, "string.slice.from.start")?
            .into_int_value();
        let negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                start_value,
                self.ctx.i64_type().const_zero(),
                "string.slice.from.negative",
            )
            .llvm_ctx("guard negative string slice-from start")?;
        let past_end = self
            .builder
            .build_int_compare(
                IntPredicate::SGT,
                start_value,
                len,
                "string.slice.from.past.end",
            )
            .llvm_ctx("guard string slice-from upper bound")?;
        let out_of_bounds = self
            .builder
            .build_or(negative, past_end, "string.slice.from.bounds")
            .llvm_ctx("combine string slice-from guards")?;
        let safe = self
            .ctx
            .append_basic_block(self.value, "string.slice.from.safe");
        let failed = self
            .ctx
            .append_basic_block(self.value, "string.slice.from.failure");
        self.builder
            .build_conditional_branch(out_of_bounds, failed, safe)
            .llvm_ctx("branch around fallible string slice-from call")?;

        self.builder.position_at_end(failed);
        self.emit_edge(failure)?;

        self.builder.position_at_end(safe);
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let function = get_or_declare_external(
            self.llvm,
            "hew_string_slice_codepoints_from",
            ptr.fn_type(&[ptr.into(), self.ctx.i64_type().into()], false),
        )?;
        let value = self.runtime_call_value(
            function,
            &[
                self.load(text, "string.slice.from.text")?.into(),
                start_value.into(),
            ],
            "string.slice.codepoints.from",
        )?;
        self.store(result, value)?;
        self.emit_result_edge(Some(result), normal)
    }
}
