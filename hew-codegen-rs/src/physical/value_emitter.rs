//! Type-recipe execution shared by function bodies and container callbacks.

use super::*;

impl<'a, 'ctx> ValueEmitter<'a, 'ctx> {
    pub(super) fn invoke_value_callback(
        &self,
        frame: Option<&coro::Frame<'ctx>>,
        ty: &ResolvedTy,
        capability: ValueCapability,
        callback: FunctionValue<'ctx>,
        arguments: &[BasicMetadataValueEnum<'ctx>],
    ) -> CodegenResult<IntValue<'ctx>> {
        if self.module.value_capabilities[&(ty.clone(), capability)].is_resumable {
            let frame = frame.ok_or_else(|| {
                CodegenError::FailClosed("suspending value operation lacks a caller frame".into())
            })?;
            suspend::invoke_child(
                self.ctx,
                self.llvm,
                self.builder,
                self.value,
                frame,
                callback,
                arguments,
            )
        } else {
            Ok(self
                .builder
                .build_call(callback, arguments, "value.call.status")
                .llvm_ctx("invoke selected value callback")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("selected value callback returned no status".into())
                })?
                .into_int_value())
        }
    }

    pub(super) fn entry_scratch(
        &self,
        ty: BasicTypeEnum<'ctx>,
        name: &str,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let prologue = self.value.get_first_basic_block().ok_or_else(|| {
            CodegenError::FailClosed("physical function has no allocation prologue".into())
        })?;
        let builder = self.ctx.create_builder();
        if let Some(terminator) = prologue.get_terminator() {
            builder.position_before(&terminator);
        } else {
            builder.position_at_end(prologue);
        }
        builder
            .build_alloca(ty, name)
            .llvm_ctx("allocate reusable physical scratch storage")
    }

    pub(super) fn write_variant_value(
        &self,
        destination: PointerValue<'ctx>,
        variant: u32,
        fields: &[BasicValueEnum<'ctx>],
        glue_id: PhysicalVariantId,
    ) -> CodegenResult<()> {
        let glue = self.variant_glue(glue_id)?;
        let layout = self.variant_layout(&glue.ty)?;
        let object = if layout.is_indirect {
            self.alloc_variant_node(layout)?
        } else {
            destination
        };
        let object_ty = llvm_type(self.ctx, &layout.object.repr)?.into_struct_type();
        let tag_ty = object_ty
            .get_field_type_at_index(0)
            .ok_or_else(|| CodegenError::FailClosed("variant object has no tag field".into()))?
            .into_int_type();
        let tag = tag_ty.const_int(u64::from(variant), false);
        let header = self
            .builder
            .build_insert_value(object_ty.const_zero(), tag, 0, "variant.make.tag")
            .llvm_ctx("write physical variant tag")?
            .into_struct_value();
        self.builder
            .build_store(object, header)
            .llvm_ctx("initialize physical variant storage")?;
        let case = glue.variants.get(variant as usize).ok_or_else(|| {
            CodegenError::FailClosed("variant construction tag is invalid".into())
        })?;
        let payload_layout = layout
            .variants
            .get(variant as usize)
            .ok_or_else(|| CodegenError::FailClosed("variant payload layout is absent".into()))?;
        if case.fields.len() != fields.len() {
            return Err(CodegenError::FailClosed(
                "variant construction field count changed after verification".into(),
            ));
        }
        if !fields.is_empty() {
            let payload_ty = llvm_type(self.ctx, &payload_layout.repr)?.into_struct_type();
            let mut payload = payload_ty.get_undef();
            for (index, field) in fields.iter().enumerate() {
                let value = *field;
                let index = u32::try_from(index).map_err(|_| {
                    CodegenError::FailClosed("variant field index exceeds u32".into())
                })?;
                payload = self
                    .builder
                    .build_insert_value(payload, value, index, "variant.make.payload")
                    .llvm_ctx("write physical variant payload field")?
                    .into_struct_value();
            }
            let payload_ptr = self.variant_payload_ptr(object, layout)?;
            self.builder
                .build_store(payload_ptr, payload)
                .llvm_ctx("store physical variant payload")?;
        }
        if layout.is_indirect {
            self.builder
                .build_store(destination, object)
                .llvm_ctx("store indirect variant node")?;
        }
        Ok(())
    }

    pub(super) fn variant_payload_ptr(
        &self,
        object: PointerValue<'ctx>,
        layout: &PhysicalVariantLayout,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let object_ty = llvm_type(self.ctx, &layout.object.repr)?.into_struct_type();
        self.builder
            .build_struct_gep(object_ty, object, 1, "variant.payload.ptr")
            .llvm_ctx("address physical variant payload")
    }

    /// The tag-and-payload object behind one enum value slot: the slot itself
    /// for a direct enum, the heap node an indirect enum slot points at.
    pub(super) fn variant_object_ptr(
        &self,
        slot: PointerValue<'ctx>,
        layout: &PhysicalVariantLayout,
    ) -> CodegenResult<PointerValue<'ctx>> {
        if !layout.is_indirect {
            return Ok(slot);
        }
        Ok(self
            .builder
            .build_load(
                self.ctx.ptr_type(AddressSpace::default()),
                slot,
                "variant.node",
            )
            .llvm_ctx("load indirect variant node")?
            .into_pointer_value())
    }

    pub(super) fn load_variant_tag(
        &self,
        object: PointerValue<'ctx>,
        layout: &PhysicalVariantLayout,
    ) -> CodegenResult<IntValue<'ctx>> {
        let object_ty = llvm_type(self.ctx, &layout.object.repr)?.into_struct_type();
        let tag_ty = object_ty
            .get_field_type_at_index(0)
            .ok_or_else(|| CodegenError::FailClosed("variant object has no tag field".into()))?;
        let tag_ptr = self
            .builder
            .build_struct_gep(object_ty, object, 0, "variant.tag.ptr")
            .llvm_ctx("address physical variant tag")?;
        Ok(self
            .builder
            .build_load(tag_ty, tag_ptr, "variant.tag")
            .llvm_ctx("read physical variant tag")?
            .into_int_value())
    }

    fn variant_node_size(
        &self,
        layout: &PhysicalVariantLayout,
    ) -> [BasicMetadataValueEnum<'ctx>; 2] {
        let size_ty = self.ctx.i64_type();
        [
            size_ty.const_int(layout.object.size, false).into(),
            size_ty
                .const_int(u64::from(layout.object.align), false)
                .into(),
        ]
    }

    /// Allocate one indirect enum node. The enum value owns it until its
    /// destructure, switch or drop glue releases it.
    fn alloc_variant_node(
        &self,
        layout: &PhysicalVariantLayout,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let size_ty = self.ctx.i64_type();
        let alloc = get_or_declare_external(
            self.llvm,
            "hew_alloc",
            self.ctx
                .ptr_type(AddressSpace::default())
                .fn_type(&[size_ty.into(), size_ty.into()], false),
        )?;
        Ok(self
            .builder
            .build_call(alloc, &self.variant_node_size(layout), "variant.node.alloc")
            .llvm_ctx("allocate indirect variant node")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("node allocation returned void".into()))?
            .into_pointer_value())
    }

    pub(super) fn free_variant_node(
        &self,
        node: PointerValue<'ctx>,
        layout: &PhysicalVariantLayout,
    ) -> CodegenResult<()> {
        let size_ty = self.ctx.i64_type();
        let dealloc = get_or_declare_external(
            self.llvm,
            "hew_dealloc",
            self.ctx.void_type().fn_type(
                &[
                    self.ctx.ptr_type(AddressSpace::default()).into(),
                    size_ty.into(),
                    size_ty.into(),
                ],
                false,
            ),
        )?;
        let [size, align] = self.variant_node_size(layout);
        self.builder
            .build_call(dealloc, &[node.into(), size, align], "")
            .llvm_ctx("release indirect variant node")?;
        Ok(())
    }

    /// Emit or reuse one internal glue function. A recursive indirect enum's
    /// recipe reaches its own glue through a call instead of unrolling.
    pub(super) fn glue_function(
        &self,
        name: &str,
        signature: FunctionType<'ctx>,
        body: impl FnOnce(&ValueEmitter<'_, 'ctx>, FunctionValue<'ctx>) -> CodegenResult<()>,
    ) -> CodegenResult<FunctionValue<'ctx>> {
        if let Some(existing) = self.llvm.get_function(name) {
            return Ok(existing);
        }
        let function = self
            .llvm
            .add_function(name, signature, Some(Linkage::Internal));
        let builder = self.ctx.create_builder();
        let entry = self.ctx.append_basic_block(function, "entry");
        let body_block = self.ctx.append_basic_block(function, "body");
        builder.position_at_end(entry);
        builder
            .build_unconditional_branch(body_block)
            .llvm_ctx("enter variant glue")?;
        builder.position_at_end(body_block);
        let emitter = ValueEmitter {
            module: self.module,
            ctx: self.ctx,
            llvm: self.llvm,
            builder: &builder,
            value: function,
            fault_sink: None,
        };
        body(&emitter, function)?;
        Ok(function)
    }

    pub(super) fn emit_invalid_variant_tag(&self) -> CodegenResult<()> {
        let trap = Intrinsic::find("llvm.trap")
            .ok_or_else(|| CodegenError::FailClosed("LLVM trap intrinsic is unavailable".into()))?
            .get_declaration(self.llvm, &[])
            .ok_or_else(|| CodegenError::FailClosed("LLVM trap declaration failed".into()))?;
        self.builder
            .build_call(trap, &[], "variant.invalid.trap")
            .llvm_ctx("emit invalid variant tag trap")?;
        self.builder
            .build_unreachable()
            .llvm_ctx("terminate invalid variant tag")?;
        Ok(())
    }

    /// Release a `#[resource]` record by calling the program's own `close`.
    ///
    /// `close` is an ordinary private callable, so it uses the private ABI:
    /// its arguments, then the caller's fault slot, returning a status.
    ///
    /// A release emitted into a frame is that frame's fault edge (D516): a
    /// failing `close` fills the frame's fault record - keeping the first
    /// fault as the primary and appending any later one as a secondary - and
    /// the release continues, so every remaining owner is still released
    /// before the frame's cleanup dispatch carries the outcome out.
    ///
    /// Release glue has no frame to report into, so it hands the fault to
    /// `hew_fault_trap`: the runtime reports the fault's own line and crashes
    /// the actor, or ends the run. A release already in progress finishes
    /// releasing what it owns first, so the raise returns here and the glue
    /// completes; `close` consumed the value either way.
    /// Release one owner by calling the exact `close` MIR named for it.
    fn emit_authored_close(
        &self,
        value: BasicValueEnum<'ctx>,
        close: CallableId,
    ) -> CodegenResult<()> {
        let callee = callable(self.module, close)?;
        let function = self
            .llvm
            .get_function(&emitted_symbol(self.module, callee))
            .ok_or_else(|| {
                CodegenError::FailClosed("authored release has no emitted close body".into())
            })?;
        let parameter = callee.params.first().ok_or_else(|| {
            CodegenError::FailClosed("authored release close takes no receiver".into())
        })?;
        let receiver: BasicMetadataValueEnum<'ctx> = match parameter.carrier {
            ParamCarrier::Direct => value.into(),
            ParamCarrier::Indirect => {
                let slot = self.entry_scratch(
                    llvm_type(self.ctx, &parameter.layout.repr)?,
                    "resource.close.receiver",
                )?;
                self.builder
                    .build_store(slot, value)
                    .llvm_ctx("stage record release receiver")?;
                slot.into()
            }
        };
        let fault = self.entry_scratch(
            self.ctx.ptr_type(AddressSpace::default()).into(),
            "resource.close.fault",
        )?;
        self.builder
            .build_store(
                fault,
                self.ctx.ptr_type(AddressSpace::default()).const_null(),
            )
            .llvm_ctx("clear record release fault slot")?;
        let status = self
            .builder
            .build_call(function, &[receiver, fault.into()], "resource.close.status")
            .llvm_ctx("call record release")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("record release returned no status".into()))?
            .into_int_value();
        let ok = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_zero(),
                "resource.close.ok",
            )
            .llvm_ctx("compare record release status")?;
        let released = self
            .ctx
            .append_basic_block(self.value, "resource.close.done");
        let failed = self
            .ctx
            .append_basic_block(self.value, "resource.close.failed");
        self.builder
            .build_conditional_branch(ok, released, failed)
            .llvm_ctx("branch on record release status")?;
        self.builder.position_at_end(failed);
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let raised = self
            .builder
            .build_load(pointer, fault, "resource.close.raised")
            .llvm_ctx("load failing record release fault")?;
        match self.fault_sink {
            Some((active_fault, active_status)) => {
                self.record_release_fault(active_fault, active_status, raised, status)?;
            }
            None => {
                let raise = get_or_declare_external(
                    self.llvm,
                    "hew_fault_trap",
                    self.ctx
                        .void_type()
                        .fn_type(&[self.ctx.i32_type().into(), pointer.into()], false),
                )?;
                self.builder
                    .build_call(
                        raise,
                        &[status.into(), raised.into()],
                        "resource.close.trap",
                    )
                    .llvm_ctx("raise failing record release fault")?;
            }
        }
        self.builder
            .build_unconditional_branch(released)
            .llvm_ctx("continue the frame's release after a failing close")?;
        self.builder.position_at_end(released);
        // D442: `close` consumes the record (`fn close(consume self)` is the
        // checker-enforced contract), so `close`'s own body already destroys
        // every member it does not move out. Also releasing the members here
        // double-frees them — this call site owns nothing further once the
        // callee returns successfully.
        Ok(())
    }

    /// Fold a failing release's fault into the frame's own fault record.
    ///
    /// The frame keeps the first fault it owns: a `close` that fails while the
    /// frame is already faulting joins that record as a secondary diagnostic,
    /// and a `close` that fails on an otherwise-normal exit installs its own
    /// fault as the frame's. `hew_fault_combine` is the one rule for both, so
    /// only the status has to choose, and it chooses the primary's.
    pub(super) fn record_release_fault(
        &self,
        active_fault: PointerValue<'ctx>,
        active_status: PointerValue<'ctx>,
        raised: BasicValueEnum<'ctx>,
        status: IntValue<'ctx>,
    ) -> CodegenResult<()> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let primary = self
            .builder
            .build_load(pointer, active_fault, "resource.close.primary")
            .llvm_ctx("load the frame's active fault")?
            .into_pointer_value();
        let primary_status = self
            .builder
            .build_load(
                self.ctx.i32_type(),
                active_status,
                "resource.close.primary.status",
            )
            .llvm_ctx("load the frame's active status")?;
        let present = self
            .builder
            .build_is_not_null(primary, "resource.close.primary.present")
            .llvm_ctx("test the frame's active fault")?;
        let no_new_fault = self
            .builder
            .build_is_null(raised.into_pointer_value(), "resource.close.raised.absent")
            .llvm_ctx("test whether release raised a fault")?;
        let keep_status = self
            .builder
            .build_or(present, no_new_fault, "resource.close.keep.status")
            .llvm_ctx("preserve an existing outcome when cleanup succeeds")?;
        let combined_status = self
            .builder
            .build_select(
                keep_status,
                primary_status,
                BasicValueEnum::from(status),
                "resource.close.combined.status",
            )
            .llvm_ctx("preserve the first fault's status")?;
        let combine = get_or_declare_external(
            self.llvm,
            "hew_fault_combine",
            pointer.fn_type(&[pointer.into(), pointer.into()], false),
        )?;
        let combined = self
            .builder
            .build_call(
                combine,
                &[primary.into(), raised.into()],
                "resource.close.combined",
            )
            .llvm_ctx("combine a failing release into the frame's fault")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("fault combine returned void".into()))?;
        self.builder
            .build_store(active_fault, combined)
            .llvm_ctx("install the frame's combined fault")?;
        self.builder
            .build_store(active_status, combined_status)
            .llvm_ctx("install the frame's combined status")?;
        Ok(())
    }

    /// Arm a release-fault sink around a release the runtime performs.
    ///
    /// A collection, shared handle, callable environment or erased vtable drop
    /// releases inside the runtime, which has no fault slot of its own to hand
    /// back. The sink is that slot: a failing `close` records against it, the
    /// release still finishes, and the fault comes back here to join the
    /// frame's own record.
    ///
    /// This adds no basic block. A release sits inside sequences whose later
    /// phi nodes name the block it was emitted into, so the bracket folds the
    /// no-fault case into the same combine rather than branching around it:
    /// `hew_fault_combine` already returns the primary for a null secondary,
    /// and the status slot starts at zero for a release that raised nothing.
    pub(super) fn emit_release_in_sink<T>(
        &self,
        emit: impl FnOnce() -> CodegenResult<T>,
    ) -> CodegenResult<T> {
        let Some((active_fault, active_status)) = self.fault_sink else {
            return emit();
        };
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let slot = self.entry_scratch(self.ctx.i32_type().into(), "release.sink.status")?;
        self.builder
            .build_store(slot, self.ctx.i32_type().const_zero())
            .llvm_ctx("clear the release sink status")?;
        let begin = get_or_declare_external(
            self.llvm,
            "hew_release_fault_begin",
            self.ctx.void_type().fn_type(&[], false),
        )?;
        self.builder
            .build_call(begin, &[], "")
            .llvm_ctx("arm a release fault sink")?;
        let output = emit()?;
        let end = get_or_declare_external(
            self.llvm,
            "hew_release_fault_end",
            pointer.fn_type(&[pointer.into()], false),
        )?;
        let raised = self
            .builder
            .build_call(end, &[slot.into()], "release.sink.fault")
            .llvm_ctx("end a release fault sink")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("release sink returned void".into()))?;
        let status = self
            .builder
            .build_load(self.ctx.i32_type(), slot, "release.sink.status.value")
            .llvm_ctx("load the released fault's status")?
            .into_int_value();
        self.record_release_fault(active_fault, active_status, raised, status)?;
        Ok(output)
    }

    pub(super) fn clone_loaded_value(
        &self,
        value: BasicValueEnum<'ctx>,
        layout: &PhysicalLayout,
        action: CloneAction,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match action {
            CloneAction::Encoding(format) => {
                let function = external_unary_ptr(
                    self.ctx,
                    self.llvm,
                    hew_mir::physical::EncodingOp::Clone.c_symbol(format),
                )?;
                self.builder
                    .build_call(
                        function,
                        &[value.into_pointer_value().into()],
                        "encoding.clone",
                    )
                    .llvm_ctx("clone encoding value")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| CodegenError::FailClosed("encoding clone returned void".into()))
            }
            CloneAction::Callable => self.clone_callable_value(value, layout),
            CloneAction::Bitwise => Ok(value),
            CloneAction::StringRetain => {
                let pointer = value.into_pointer_value();
                let function = external_unary_ptr(self.ctx, self.llvm, "hew_string_clone")?;
                self.builder
                    .build_call(function, &[pointer.into()], "string.retain")
                    .llvm_ctx("retain physical string")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| CodegenError::FailClosed("string retain returned void".into()))
            }
            CloneAction::RcRetain | CloneAction::WeakRetain => {
                let symbol = if action == CloneAction::RcRetain {
                    RuntimeCallFamily::RcClone.row().symbol
                } else {
                    RuntimeCallFamily::WeakCloneRc.row().symbol
                };
                let function = external_unary_ptr(self.ctx, self.llvm, symbol)?;
                self.builder
                    .build_call(
                        function,
                        &[value.into_pointer_value().into()],
                        "shared.retain",
                    )
                    .llvm_ctx("retain a shared allocation")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| CodegenError::FailClosed("shared retain returned void".into()))
            }
            CloneAction::BytesRetain => {
                let aggregate = value.into_struct_value();
                let pointer = self
                    .builder
                    .build_extract_value(aggregate, 0, "bytes.ptr")
                    .llvm_ctx("extract physical bytes pointer")?
                    .into_pointer_value();
                let ptr = self.ctx.ptr_type(AddressSpace::default());
                let function = get_or_declare_external(
                    self.llvm,
                    "hew_bytes_clone_ref",
                    self.ctx.void_type().fn_type(&[ptr.into()], false),
                )?;
                self.builder
                    .build_call(function, &[pointer.into()], "bytes.retain")
                    .llvm_ctx("retain physical bytes")?;
                Ok(aggregate.into())
            }
            CloneAction::Aggregate(id) => {
                let glue = self.aggregate_glue(id)?;
                let source = value.into_struct_value();
                let BasicTypeEnum::StructType(aggregate_ty) = llvm_type(self.ctx, &layout.repr)?
                else {
                    return Err(CodegenError::FailClosed(
                        "physical aggregate clone has a non-struct layout".into(),
                    ));
                };
                let mut clone = aggregate_ty.get_undef();
                for (index, field) in glue.fields.iter().enumerate() {
                    let index = u32::try_from(index).map_err(|_| {
                        CodegenError::FailClosed(
                            "physical aggregate field index exceeds u32".into(),
                        )
                    })?;
                    let value = self
                        .builder
                        .build_extract_value(source, index, "aggregate.clone.field")
                        .llvm_ctx("extract physical aggregate clone field")?;
                    let action = field.clone.ok_or_else(|| {
                        CodegenError::FailClosed(format!(
                            "physical aggregate glue {} field {index} has no clone action",
                            id.0
                        ))
                    })?;
                    let layout = self.module.target.layout(&field.ty).ok_or_else(|| {
                        CodegenError::FailClosed(format!(
                            "physical aggregate glue {} field {index} has no target layout",
                            id.0
                        ))
                    })?;
                    let value = self.clone_loaded_value(value, layout, action)?;
                    clone = match self
                        .builder
                        .build_insert_value(clone, value, index, "aggregate.clone")
                        .llvm_ctx("insert physical aggregate clone field")?
                    {
                        inkwell::values::AggregateValueEnum::StructValue(value) => value,
                        inkwell::values::AggregateValueEnum::ArrayValue(_) => {
                            return Err(CodegenError::FailClosed(
                                "physical aggregate clone produced an LLVM array".into(),
                            ));
                        }
                    };
                }
                Ok(clone.into())
            }
            CloneAction::Variant(id) => self.clone_variant_value(value, layout, id),
            CloneAction::Array(_)
            | CloneAction::Vector(_)
            | CloneAction::Map(_)
            | CloneAction::Set(_) => {
                let symbol = match action {
                    CloneAction::Array(id) => {
                        self.vector_glue(id)?;
                        "hew_array_clone"
                    }
                    CloneAction::Vector(id) => {
                        self.vector_glue(id)?;
                        "hew_vec_clone_owned"
                    }
                    CloneAction::Map(id) => {
                        self.module
                            .map_glue
                            .get(id.0 as usize)
                            .filter(|glue| glue.id == id)
                            .ok_or_else(|| {
                                CodegenError::FailClosed("unknown physical map glue".into())
                            })?;
                        "hew_hashmap_clone_layout"
                    }
                    CloneAction::Set(id) => {
                        self.module
                            .set_glue
                            .get(id.0 as usize)
                            .filter(|glue| glue.id == id)
                            .ok_or_else(|| {
                                CodegenError::FailClosed("unknown physical set glue".into())
                            })?;
                        "hew_hashset_clone_layout"
                    }
                    _ => unreachable!("matched collection clone"),
                };
                let function = external_unary_ptr(self.ctx, self.llvm, symbol)?;
                self.builder
                    .build_call(function, &[value.into()], "collection.clone")
                    .llvm_ctx("clone descriptor-backed collection")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| {
                        CodegenError::FailClosed("collection clone returned void".into())
                    })
            }
        }
    }

    pub(super) fn destroy_loaded_value(
        &self,
        value: BasicValueEnum<'ctx>,
        layout: &PhysicalLayout,
        action: DestroyAction,
    ) -> CodegenResult<()> {
        match action {
            DestroyAction::Resource(id) => {
                let resource = self.module.resources.get(id.0 as usize).ok_or_else(|| {
                    CodegenError::FailClosed("resource drop lacks its verified contract".into())
                })?;
                if let hew_mir::physical::ResourceRelease::RecordClose { close, .. }
                | hew_mir::physical::ResourceRelease::OpaqueClose { close, .. } =
                    &resource.release
                {
                    return self.emit_authored_close(value, *close);
                }
                let symbol = resource
                    .release
                    .release_symbol()
                    .map_err(CodegenError::FailClosed)?;
                let result = resource.release.release_result();
                let parameters = [value.get_type().into()];
                let signature = if result == ResolvedTy::Unit {
                    self.ctx.void_type().fn_type(&parameters, false)
                } else {
                    let result_layout = self.module.target.layout(&result).ok_or_else(|| {
                        CodegenError::FailClosed(
                            "resource release result lacks its ABI layout".into(),
                        )
                    })?;
                    llvm_type(self.ctx, &result_layout.repr)?.fn_type(&parameters, false)
                };
                let function = get_or_declare_external(self.llvm, symbol, signature)?;
                self.builder
                    .build_call(function, &[value.into()], "")
                    .llvm_ctx("release resource owner")?;
                Ok(())
            }
            DestroyAction::TraitObject => self.destroy_trait_object(value),
            DestroyAction::Callable => {
                let slot =
                    self.entry_scratch(llvm_type(self.ctx, &layout.repr)?, "callable.drop.slot")?;
                self.builder
                    .build_store(slot, value)
                    .llvm_ctx("stage callable destruction")?;
                let drop = external_drop(self.ctx, self.llvm, "hew_callable_drop")?;
                self.emit_release_in_sink(|| {
                    self.builder
                        .build_call(drop, &[slot.into()], "")
                        .llvm_ctx("destroy callable environment")?;
                    Ok(())
                })
            }

            // Releasing a strong handle may run the payload's own release,
            // which the runtime already holds as this allocation's destructor.
            DestroyAction::RcRelease(_) | DestroyAction::WeakRelease => {
                let symbol = if matches!(action, DestroyAction::WeakRelease) {
                    RuntimeCallFamily::WeakDropRc.row().symbol
                } else {
                    RuntimeCallFamily::RcDrop.row().symbol
                };
                let function = external_drop(self.ctx, self.llvm, symbol)?;
                self.emit_release_in_sink(|| {
                    self.builder
                        .build_call(function, &[value.into_pointer_value().into()], "")
                        .llvm_ctx("release a shared allocation")?;
                    Ok(())
                })
            }
            DestroyAction::Encoding(_)
            | DestroyAction::StringRelease
            | DestroyAction::BytesRelease => {
                let pointer = match action {
                    DestroyAction::Encoding(_) | DestroyAction::StringRelease => {
                        value.into_pointer_value()
                    }
                    DestroyAction::BytesRelease => self
                        .builder
                        .build_extract_value(value.into_struct_value(), 0, "bytes.drop.ptr")
                        .llvm_ctx("extract bytes release pointer")?
                        .into_pointer_value(),
                    DestroyAction::Resource(_)
                    | DestroyAction::Callable
                    | DestroyAction::TraitObject
                    | DestroyAction::RcRelease(_)
                    | DestroyAction::WeakRelease
                    | DestroyAction::Aggregate(_) => {
                        unreachable!("matched primitive release")
                    }
                    DestroyAction::Variant(_) => unreachable!("matched primitive release"),
                    DestroyAction::Array(_)
                    | DestroyAction::Vector(_)
                    | DestroyAction::Map(_)
                    | DestroyAction::Set(_) => {
                        unreachable!("matched primitive release")
                    }
                };
                let symbol = match action {
                    DestroyAction::Encoding(format) => {
                        hew_mir::physical::EncodingOp::Free.c_symbol(format)
                    }
                    DestroyAction::StringRelease => "hew_string_drop",
                    DestroyAction::BytesRelease => "hew_bytes_drop",
                    DestroyAction::Resource(_)
                    | DestroyAction::Callable
                    | DestroyAction::TraitObject
                    | DestroyAction::RcRelease(_)
                    | DestroyAction::WeakRelease
                    | DestroyAction::Aggregate(_) => {
                        unreachable!("matched primitive release")
                    }
                    DestroyAction::Variant(_) => unreachable!("matched primitive release"),
                    DestroyAction::Array(_)
                    | DestroyAction::Vector(_)
                    | DestroyAction::Map(_)
                    | DestroyAction::Set(_) => {
                        unreachable!("matched primitive release")
                    }
                };
                let function = external_drop(self.ctx, self.llvm, symbol)?;
                self.builder
                    .build_call(function, &[pointer.into()], "physical.drop")
                    .llvm_ctx("release physical owner")?;
                Ok(())
            }
            DestroyAction::Aggregate(id) => {
                let glue = self.aggregate_glue(id)?;
                let value = value.into_struct_value();
                let PhysicalRepr::Struct(layout_fields) = &layout.repr else {
                    return Err(CodegenError::FailClosed(
                        "physical aggregate destroy has a non-struct layout".into(),
                    ));
                };
                for index in (0..glue.fields.len()).rev() {
                    let field = &glue.fields[index];
                    let Some(action) = field.destroy else {
                        continue;
                    };
                    let index = u32::try_from(index).map_err(|_| {
                        CodegenError::FailClosed(
                            "physical aggregate field index exceeds u32".into(),
                        )
                    })?;
                    let field_value = self
                        .builder
                        .build_extract_value(value, index, "aggregate.destroy.field")
                        .llvm_ctx("extract physical aggregate destroy field")?;
                    self.destroy_loaded_value(field_value, &layout_fields[index as usize], action)?;
                }
                Ok(())
            }
            DestroyAction::Variant(id) => self.destroy_variant_value(value, layout, id),
            DestroyAction::Array(_)
            | DestroyAction::Vector(_)
            | DestroyAction::Map(_)
            | DestroyAction::Set(_) => {
                // Physical MIR decides whether this release runs any
                // user-visible action. When it does not, the `_walk` entry
                // joins a release already in progress instead of nesting one
                // native frame per level; anything that can reach a resource
                // drains synchronously so its `close` keeps today's order.
                let walk = !self.module.releases.runs_user_code(action);
                let symbol = match action {
                    DestroyAction::Array(id) => {
                        self.vector_glue(id)?;
                        if walk {
                            "hew_array_free_walk"
                        } else {
                            "hew_array_free"
                        }
                    }
                    DestroyAction::Vector(id) => {
                        self.vector_glue(id)?;
                        if walk {
                            "hew_vec_free_owned_walk"
                        } else {
                            "hew_vec_free_owned"
                        }
                    }
                    DestroyAction::Map(id) => {
                        self.module
                            .map_glue
                            .get(id.0 as usize)
                            .filter(|glue| glue.id == id)
                            .ok_or_else(|| {
                                CodegenError::FailClosed("unknown physical map glue".into())
                            })?;
                        if walk {
                            "hew_hashmap_free_layout_walk"
                        } else {
                            "hew_hashmap_free_layout"
                        }
                    }
                    DestroyAction::Set(id) => {
                        self.module
                            .set_glue
                            .get(id.0 as usize)
                            .filter(|glue| glue.id == id)
                            .ok_or_else(|| {
                                CodegenError::FailClosed("unknown physical set glue".into())
                            })?;
                        if walk {
                            "hew_hashset_free_layout_walk"
                        } else {
                            "hew_hashset_free_layout"
                        }
                    }
                    _ => unreachable!("matched collection destroy"),
                };
                let function = external_drop(self.ctx, self.llvm, symbol)?;
                self.emit_release_in_sink(|| {
                    self.builder
                        .build_call(function, &[value.into()], "collection.drop")
                        .llvm_ctx("destroy descriptor-backed collection")?;
                    Ok(())
                })
            }
        }
    }

    fn clone_variant_value(
        &self,
        value: BasicValueEnum<'ctx>,
        _layout: &PhysicalLayout,
        id: PhysicalVariantId,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let glue = self.variant_glue(id)?;
        let variant_layout = self.variant_layout(&glue.ty)?;
        if variant_layout.is_indirect {
            let pointer = self.ctx.ptr_type(AddressSpace::default());
            let clone = self.glue_function(
                &format!("__hew_variant_clone_{}", id.0),
                pointer.fn_type(&[pointer.into()], false),
                |emitter, function| {
                    let source = function
                        .get_nth_param(0)
                        .ok_or_else(|| {
                            CodegenError::FailClosed("variant clone lacks its node".into())
                        })?
                        .into_pointer_value();
                    let destination = emitter.alloc_variant_node(variant_layout)?;
                    emitter.clone_variant_object(source, destination, glue, variant_layout)?;
                    emitter
                        .builder
                        .build_return(Some(&destination))
                        .llvm_ctx("finish indirect variant clone")?;
                    Ok(())
                },
            )?;
            return self
                .builder
                .build_call(clone, &[value.into()], "variant.clone.node")
                .llvm_ctx("clone indirect variant")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed("variant clone returned void".into()));
        }
        let object_ty = llvm_type(self.ctx, &variant_layout.object.repr)?.into_struct_type();
        let source = self.entry_scratch(object_ty.into(), "variant.clone.source")?;
        let destination = self.entry_scratch(object_ty.into(), "variant.clone.destination")?;
        self.builder
            .build_store(source, value)
            .llvm_ctx("store physical variant clone source")?;
        self.clone_variant_object(source, destination, glue, variant_layout)?;
        self.builder
            .build_load(object_ty, destination, "variant.clone.result")
            .llvm_ctx("load cloned physical variant")
    }

    /// Copy the active case of `source` into the uninitialized `destination`
    /// object; both are tag-and-payload objects of one variant layout.
    fn clone_variant_object(
        &self,
        source: PointerValue<'ctx>,
        destination: PointerValue<'ctx>,
        glue: &PhysicalVariantGlue,
        variant_layout: &PhysicalVariantLayout,
    ) -> CodegenResult<()> {
        let id = glue.id;
        let object_ty = llvm_type(self.ctx, &variant_layout.object.repr)?.into_struct_type();
        let tag = self.load_variant_tag(source, variant_layout)?;
        let invalid = self
            .ctx
            .append_basic_block(self.value, "variant.clone.invalid");
        let complete = self
            .ctx
            .append_basic_block(self.value, "variant.clone.complete");
        let cases = glue
            .variants
            .iter()
            .enumerate()
            .map(|(index, _)| {
                (
                    tag.get_type().const_int(index as u64, false),
                    self.ctx
                        .append_basic_block(self.value, &format!("variant.clone.case.{index}")),
                )
            })
            .collect::<Vec<_>>();
        self.builder
            .build_switch(tag, invalid, &cases)
            .llvm_ctx("dispatch physical variant clone")?;
        for (index, (_, case_block)) in cases.iter().enumerate() {
            self.builder.position_at_end(*case_block);
            let tag_value = tag.get_type().const_int(index as u64, false);
            let object = self
                .builder
                .build_insert_value(object_ty.const_zero(), tag_value, 0, "variant.clone.object")
                .llvm_ctx("write cloned physical variant tag")?
                .into_struct_value();
            self.builder
                .build_store(destination, object)
                .llvm_ctx("initialize cloned physical variant")?;
            let recipe = &glue.variants[index];
            if !recipe.fields.is_empty() {
                let payload_layout = &variant_layout.variants[index];
                let payload_ty = llvm_type(self.ctx, &payload_layout.repr)?.into_struct_type();
                let source_ptr = self.variant_payload_ptr(source, variant_layout)?;
                let source_payload = self
                    .builder
                    .build_load(payload_ty, source_ptr, "variant.clone.payload")
                    .llvm_ctx("load physical variant clone payload")?
                    .into_struct_value();
                let mut destination_payload = payload_ty.get_undef();
                for (field_index, field) in recipe.fields.iter().enumerate() {
                    let field_index = u32::try_from(field_index).map_err(|_| {
                        CodegenError::FailClosed("variant clone field index exceeds u32".into())
                    })?;
                    let field_value = self
                        .builder
                        .build_extract_value(source_payload, field_index, "variant.clone.field")
                        .llvm_ctx("extract physical variant clone field")?;
                    let action = field.clone.ok_or_else(|| {
                        CodegenError::FailClosed(format!(
                            "physical variant glue {} case {index} field {field_index} has no clone action",
                            id.0
                        ))
                    })?;
                    let field_layout = self.module.target.layout(&field.ty).ok_or_else(|| {
                        CodegenError::FailClosed("variant clone field has no target layout".into())
                    })?;
                    let cloned = self.clone_loaded_value(field_value, field_layout, action)?;
                    destination_payload = self
                        .builder
                        .build_insert_value(
                            destination_payload,
                            cloned,
                            field_index,
                            "variant.clone.payload.result",
                        )
                        .llvm_ctx("insert physical variant clone field")?
                        .into_struct_value();
                }
                let destination_ptr = self.variant_payload_ptr(destination, variant_layout)?;
                self.builder
                    .build_store(destination_ptr, destination_payload)
                    .llvm_ctx("store cloned physical variant payload")?;
            }
            self.builder
                .build_unconditional_branch(complete)
                .llvm_ctx("finish physical variant clone case")?;
        }
        self.builder.position_at_end(invalid);
        self.emit_invalid_variant_tag()?;
        self.builder.position_at_end(complete);
        Ok(())
    }

    fn destroy_variant_value(
        &self,
        value: BasicValueEnum<'ctx>,
        _layout: &PhysicalLayout,
        id: PhysicalVariantId,
    ) -> CodegenResult<()> {
        let glue = self.variant_glue(id)?;
        let variant_layout = self.variant_layout(&glue.ty)?;
        if variant_layout.is_indirect {
            let pointer = self.ctx.ptr_type(AddressSpace::default());
            let drop = self.glue_function(
                &format!("__hew_variant_drop_{}", id.0),
                self.ctx.void_type().fn_type(&[pointer.into()], false),
                |emitter, function| {
                    let node = function
                        .get_nth_param(0)
                        .ok_or_else(|| {
                            CodegenError::FailClosed("variant drop lacks its node".into())
                        })?
                        .into_pointer_value();
                    emitter.destroy_variant_object(node, glue, variant_layout)?;
                    emitter.free_variant_node(node, variant_layout)?;
                    emitter
                        .builder
                        .build_return(None)
                        .llvm_ctx("finish indirect variant drop")?;
                    Ok(())
                },
            )?;
            self.builder
                .build_call(drop, &[value.into()], "")
                .llvm_ctx("destroy indirect variant")?;
            return Ok(());
        }
        let object_ty = llvm_type(self.ctx, &variant_layout.object.repr)?.into_struct_type();
        let source = self.entry_scratch(object_ty.into(), "variant.destroy.source")?;
        self.builder
            .build_store(source, value)
            .llvm_ctx("store physical variant destroy source")?;
        self.destroy_variant_object(source, glue, variant_layout)
    }

    /// Release the owned fields of the active case in `source`, a
    /// tag-and-payload object; the object's own storage stays the caller's.
    fn destroy_variant_object(
        &self,
        source: PointerValue<'ctx>,
        glue: &PhysicalVariantGlue,
        variant_layout: &PhysicalVariantLayout,
    ) -> CodegenResult<()> {
        let tag = self.load_variant_tag(source, variant_layout)?;
        let invalid = self
            .ctx
            .append_basic_block(self.value, "variant.destroy.invalid");
        let complete = self
            .ctx
            .append_basic_block(self.value, "variant.destroy.complete");
        let cases = glue
            .variants
            .iter()
            .enumerate()
            .map(|(index, _)| {
                (
                    tag.get_type().const_int(index as u64, false),
                    self.ctx
                        .append_basic_block(self.value, &format!("variant.destroy.case.{index}")),
                )
            })
            .collect::<Vec<_>>();
        self.builder
            .build_switch(tag, invalid, &cases)
            .llvm_ctx("dispatch physical variant destroy")?;
        for (index, (_, case_block)) in cases.iter().enumerate() {
            self.builder.position_at_end(*case_block);
            let recipe = &glue.variants[index];
            if !recipe.fields.is_empty() {
                let payload_layout = &variant_layout.variants[index];
                let payload_ty = llvm_type(self.ctx, &payload_layout.repr)?.into_struct_type();
                let source_ptr = self.variant_payload_ptr(source, variant_layout)?;
                let payload = self
                    .builder
                    .build_load(payload_ty, source_ptr, "variant.destroy.payload")
                    .llvm_ctx("load physical variant destroy payload")?
                    .into_struct_value();
                for field_index in (0..recipe.fields.len()).rev() {
                    let field = &recipe.fields[field_index];
                    let Some(action) = field.destroy else {
                        continue;
                    };
                    let field_index = u32::try_from(field_index).map_err(|_| {
                        CodegenError::FailClosed("variant destroy field index exceeds u32".into())
                    })?;
                    let field_value = self
                        .builder
                        .build_extract_value(payload, field_index, "variant.destroy.field")
                        .llvm_ctx("extract physical variant destroy field")?;
                    let field_layout = self.module.target.layout(&field.ty).ok_or_else(|| {
                        CodegenError::FailClosed(
                            "variant destroy field has no target layout".into(),
                        )
                    })?;
                    self.destroy_loaded_value(field_value, field_layout, action)?;
                }
            }
            self.builder
                .build_unconditional_branch(complete)
                .llvm_ctx("finish physical variant destroy case")?;
        }
        self.builder.position_at_end(invalid);
        self.emit_invalid_variant_tag()?;
        self.builder.position_at_end(complete);
        Ok(())
    }

    pub(super) fn aggregate_glue(
        &self,
        id: PhysicalAggregateId,
    ) -> CodegenResult<&'a PhysicalAggregateGlue> {
        self.module
            .aggregate_glue
            .get(id.0 as usize)
            .filter(|glue| glue.id == id)
            .ok_or_else(|| {
                CodegenError::FailClosed(format!("unknown physical aggregate glue {}", id.0))
            })
    }

    pub(super) fn vector_glue(
        &self,
        id: PhysicalVectorId,
    ) -> CodegenResult<&'a PhysicalVectorGlue> {
        self.module
            .vector_glue
            .get(id.0 as usize)
            .filter(|glue| glue.id == id)
            .ok_or_else(|| {
                CodegenError::FailClosed(format!("unknown physical vector glue {}", id.0))
            })
    }

    pub(super) fn variant_glue(
        &self,
        id: PhysicalVariantId,
    ) -> CodegenResult<&'a PhysicalVariantGlue> {
        self.module
            .variant_glue
            .get(id.0 as usize)
            .filter(|glue| glue.id == id)
            .ok_or_else(|| {
                CodegenError::FailClosed(format!("unknown physical variant glue {}", id.0))
            })
    }

    pub(super) fn variant_layout(
        &self,
        ty: &ResolvedTy,
    ) -> CodegenResult<&'a PhysicalVariantLayout> {
        self.module.target.variant_layout(ty).ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "physical variant `{}` has no target layout",
                ty.user_facing()
            ))
        })
    }
}

pub(super) fn vector_descriptor_symbol(id: PhysicalVectorId) -> String {
    format!("__hew_vector_element_layout_{}", id.0)
}

/// The payload destructor one shared allocation installs at construction.
pub(super) fn shared_payload_drop_symbol(id: hew_mir::physical::PhysicalSharedId) -> String {
    format!("__hew_shared_payload_{}_drop", id.0)
}

pub(super) fn map_key_descriptor_symbol(id: PhysicalMapId) -> String {
    format!("__hew_map_key_{}", id.0)
}

pub(super) fn map_value_descriptor_symbol(id: PhysicalMapId) -> String {
    format!("__hew_map_value_{}", id.0)
}

pub(super) fn set_key_descriptor_symbol(id: PhysicalSetId) -> String {
    format!("__hew_set_key_{}", id.0)
}

pub(super) fn value_descriptor_type<'ctx>(
    ctx: &'ctx Context,
    target: &TargetData,
) -> inkwell::types::StructType<'ctx> {
    let size_ty = ctx.ptr_sized_int_type(target, None);
    let pointer = ctx.ptr_type(AddressSpace::default());
    // HewValueLayout's C layout is realized for the selected target,
    // including padding around its u8 ownership discriminant.
    ctx.struct_type(
        &[
            size_ty.into(),
            size_ty.into(),
            ctx.i8_type().into(),
            pointer.into(),
            pointer.into(),
            pointer.into(),
        ],
        false,
    )
}
