//! Function-body emission: frames, storage, operations and value recipes.

use super::*;

impl<'a, 'ctx> FunctionEmitter<'a, 'ctx> {
    pub(super) fn value_emitter(&self) -> ValueEmitter<'_, 'ctx> {
        ValueEmitter {
            module: self.module,
            ctx: self.ctx,
            llvm: self.llvm,
            builder: &self.builder,
            value: self.value,
            fault_sink: Some((self.active_fault, self.active_status)),
        }
    }

    pub(super) fn new(
        module: &'a ModuleEmitter<'ctx, '_>,
        function: &'a PhysicalFunction,
        callable: &PhysicalCallable,
        value: FunctionValue<'ctx>,
    ) -> CodegenResult<Self> {
        let ctx = module.ctx;
        let builder = ctx.create_builder();
        let prologue = ctx.append_basic_block(value, "physical.prologue");
        builder.position_at_end(prologue);
        // A body with debug info keeps a current location for every
        // instruction it builds: LLVM requires one on each inlinable call.
        let debug = match (
            &module.debug,
            module.module.debug.functions.get(&callable.id),
        ) {
            (Some(emitter), Some(attribution)) => {
                let function_debug = emitter.function(value, &callable.symbol, attribution);
                builder.set_current_debug_location(emitter.declaration_location(
                    ctx,
                    &function_debug,
                    attribution.decl,
                ));
                debug::pin_for_inspection(ctx, value, callable.is_resumable);
                Some((emitter, function_debug, attribution))
            }
            _ => None,
        };
        let frame = if callable.is_resumable {
            let state = value
                .get_last_param()
                .ok_or_else(|| {
                    CodegenError::FailClosed("resumable body has no invocation state".into())
                })?
                .into_pointer_value();
            Some(coro::begin(ctx, &module.llvm, &builder, value, state)?)
        } else {
            None
        };
        let slots = partial::allocate_storage(module, function, callable, value, &builder)?;
        let pending_locals = match &debug {
            Some((emitter, function_debug, attribution)) => debug::declare_locals(
                ctx,
                emitter,
                function_debug,
                attribution,
                function,
                &module.module.target,
                &slots,
                prologue,
                callable.is_resumable,
            ),
            None => Vec::new(),
        };
        let place_flags = partial::allocate_flags(module, function, &builder, &slots)?;
        let active_fault = builder
            .build_alloca(ctx.ptr_type(AddressSpace::default()), "active.fault")
            .llvm_ctx("allocate active fault")?;
        let active_status = builder
            .build_alloca(ctx.i32_type(), "active.status")
            .llvm_ctx("allocate active status")?;
        builder
            .build_store(
                active_fault,
                ctx.ptr_type(AddressSpace::default()).const_null(),
            )
            .llvm_ctx("initialize active fault")?;
        builder
            .build_store(active_status, ctx.i32_type().const_zero())
            .llvm_ctx("initialize active status")?;

        let mut fault_parks = BTreeMap::new();
        for block in &function.blocks {
            if let PhysicalTerminator::EnterDefer { park, .. } = block.terminator {
                if let std::collections::btree_map::Entry::Vacant(entry) = fault_parks.entry(park) {
                    let pointer = builder
                        .build_alloca(
                            ctx.ptr_type(AddressSpace::default()),
                            &format!("park.{}.fault", park.0),
                        )
                        .llvm_ctx("allocate defer fault park")?;
                    let status = builder
                        .build_alloca(ctx.i32_type(), &format!("park.{}.status", park.0))
                        .llvm_ctx("allocate defer status park")?;
                    builder
                        .build_store(pointer, ctx.ptr_type(AddressSpace::default()).const_null())
                        .llvm_ctx("initialize defer fault park")?;
                    builder
                        .build_store(status, ctx.i32_type().const_zero())
                        .llvm_ctx("initialize defer status park")?;
                    entry.insert((pointer, status));
                }
            }
        }
        let mut task_scopes = BTreeMap::new();
        for op in function.blocks.iter().flat_map(|block| &block.ops) {
            if let PhysicalOp::TaskScopeEnter { scope, .. } = op {
                let slot = builder
                    .build_alloca(
                        ctx.ptr_type(AddressSpace::default()),
                        &format!("task.scope.{}", scope.0),
                    )
                    .llvm_ctx("allocate task scope slot")?;
                task_scopes.insert(*scope, slot);
            }
        }
        let mut param_index = 0u32;
        for ((parameter, storage_id), physical_param) in value
            .get_params()
            .into_iter()
            .zip(&function.parameters)
            .zip(&callable.params)
        {
            if physical_param.passing == hew_mir::physical::SemParamPassing::BorrowMut {
                param_index += 1;
                continue;
            }
            let loaded = match physical_param.carrier {
                ParamCarrier::Direct => parameter,
                ParamCarrier::Indirect => builder
                    .build_load(
                        llvm_type(ctx, &physical_param.layout.repr)?,
                        parameter.into_pointer_value(),
                        "param.indirect",
                    )
                    .llvm_ctx("load indirect physical parameter")?,
            };
            builder
                .build_store(slots[storage_id.0 as usize], loaded)
                .llvm_ctx("store physical parameter")?;
            param_index += 1;
        }
        let result_out = if callable.return_layout.is_some() {
            let result = value
                .get_nth_param(param_index)
                .ok_or_else(|| CodegenError::FailClosed("missing result-out parameter".into()))?
                .into_pointer_value();
            param_index += 1;
            Some(result)
        } else {
            None
        };
        let fault_out = value
            .get_nth_param(param_index)
            .ok_or_else(|| CodegenError::FailClosed("missing fault-out parameter".into()))?
            .into_pointer_value();
        let blocks = function
            .blocks
            .iter()
            .map(|block| {
                (
                    block.id,
                    ctx.append_basic_block(value, &format!("bb{}", block.id.0)),
                )
            })
            .collect::<BTreeMap<_, _>>();
        builder
            .build_unconditional_branch(blocks[&function.entry])
            .llvm_ctx("branch to physical entry")?;
        Ok(Self {
            module: module.module,
            function,
            ctx,
            llvm: &module.llvm,
            builder,
            value,
            blocks,
            slots,
            place_flags,
            result_out,
            fault_out,
            active_fault,
            active_status,
            fault_parks,
            functions: &module.functions,
            value_callbacks: &module.value_callbacks,
            ramps: &module.ramps,
            frame,
            task_scopes,
            debug,
            prologue,
            pending_locals,
        })
    }

    pub(super) fn emit(self) -> CodegenResult<()> {
        let inspectable = self.debug.is_some();
        let value = self.value;
        for block in &self.function.blocks {
            self.builder.position_at_end(self.blocks[&block.id]);
            // Blocks are emitted in id order, not execution order, so a block
            // opens on its own first source point rather than inheriting the
            // line of whichever block was emitted before it.
            self.enter_block(block.id);
            for (index, operation) in block.ops.iter().enumerate() {
                self.locate(block.id, index);
                self.emit_op(operation)?;
            }
            self.locate(block.id, block.ops.len());
            self.emit_terminator(block)?;
        }
        if let Some((emitter, _, _)) = &self.debug {
            debug::resolve_coroutine_locals(
                emitter,
                self.llvm,
                value,
                self.prologue,
                &self.pending_locals,
            );
        }
        if inspectable {
            debug::order_blocks_for_inspection(value);
        }
        Ok(())
    }

    /// Open a block at the earliest source point anything in it names, or at
    /// the body's declaration when it names none.
    fn enter_block(&self, block: BlockId) {
        let Some((emitter, function_debug, attribution)) = &self.debug else {
            return;
        };
        let first = attribution
            .sites
            .range((block, 0)..=(block, u32::MAX))
            .next()
            .map(|(_, offset)| *offset);
        let location = first.map_or_else(
            || emitter.declaration_location(self.ctx, function_debug, attribution.decl),
            |offset| emitter.location(self.ctx, function_debug, offset),
        );
        self.builder.set_current_debug_location(location);
    }

    /// Point the builder at the source this operation lowered from. An
    /// operation with no source point keeps the location of the last one in
    /// this block that had one.
    fn locate(&self, block: BlockId, index: usize) {
        let Some((emitter, function_debug, attribution)) = &self.debug else {
            return;
        };
        let Ok(index) = u32::try_from(index) else {
            return;
        };
        let Some(offset) = attribution.sites.get(&(block, index)) else {
            return;
        };
        self.builder.set_current_debug_location(emitter.location(
            self.ctx,
            function_debug,
            *offset,
        ));
    }

    pub(super) fn load(&self, id: StorageId, name: &str) -> CodegenResult<BasicValueEnum<'ctx>> {
        self.builder
            .build_load(
                llvm_type(self.ctx, &self.storage(id)?.layout.repr)?,
                self.slots[id.0 as usize],
                name,
            )
            .llvm_ctx("load physical storage")
    }

    pub(super) fn storage(&self, id: StorageId) -> CodegenResult<&PhysicalStorage> {
        self.function
            .storage
            .get(id.0 as usize)
            .filter(|storage| storage.id == id)
            .ok_or_else(|| CodegenError::FailClosed(format!("unknown physical storage {}", id.0)))
    }

    pub(super) fn store(&self, id: StorageId, value: BasicValueEnum<'ctx>) -> CodegenResult<()> {
        let expected = llvm_type(self.ctx, &self.storage(id)?.layout.repr)?;
        if value.get_type() != expected {
            return Err(CodegenError::FailClosed(format!(
                "physical storage {} expects {}, received {}",
                id.0,
                expected.print_to_string(),
                value.get_type().print_to_string()
            )));
        }
        self.builder
            .build_store(self.slots[id.0 as usize], value)
            .llvm_ctx("store physical storage")?;
        self.set_capture_initialized(id, true)?;
        self.set_place_initialized(id, true)?;
        Ok(())
    }

    pub(super) fn clear_owned(&self, id: StorageId) -> CodegenResult<()> {
        self.set_capture_initialized(id, false)?;
        self.set_place_initialized(id, false)?;
        // A local aggregate leaf records its transfer in this frame's
        // initialization flag, which the same frame reads back before any
        // release. An actor state seat outlives the handler: its release
        // callback reads the field itself, so a take leaves an empty carrier
        // behind and a handler that faults before publishing releases nothing
        // twice.
        if self.function.place_storage.contains_key(&id)
            && !matches!(
                self.storage(id)?.origin,
                hew_mir::physical::StorageOrigin::ActorState { .. }
            )
        {
            return Ok(());
        }
        if self.storage(id)?.own == OwnKind::Owned {
            let zero = llvm_type(self.ctx, &self.storage(id)?.layout.repr)?.const_zero();
            self.builder
                .build_store(self.slots[id.0 as usize], zero)
                .llvm_ctx("clear transferred physical owner")?;
        }
        Ok(())
    }

    #[allow(
        clippy::too_many_lines,
        reason = "the physical operation match is deliberately exhaustive and contains no ownership selection"
    )]
    fn emit_op(&self, operation: &PhysicalOp) -> CodegenResult<()> {
        match operation {
            PhysicalOp::GeneratorMake { callable, dest, .. } => {
                self.emit_generator_make(*callable, *dest)
            }
            PhysicalOp::StreamPipe {
                capacity,
                stream,
                sink,
                ..
            } => self.emit_stream_pipe(*capacity, *stream, *sink),
            PhysicalOp::RegisterDefer { .. } => Ok(()),
            PhysicalOp::FunctionMake { dest, callee } => self.emit_function_make(*dest, *callee),
            PhysicalOp::TaskScopeEnter {
                scope,
                parent,
                duration,
            } => self.emit_task_scope_enter(*scope, *parent, *duration),
            PhysicalOp::TaskScopeClose { scope } => self.emit_task_scope_close(*scope),
            PhysicalOp::TaskSpawn {
                scope,
                callable,
                dest,
                ..
            } => self.emit_task_spawn(*scope, *callable, *dest),
            PhysicalOp::ClosureMake {
                dest,
                closure,
                fields,
            } => self.emit_closure_make(*dest, *closure, fields),
            PhysicalOp::DynMake {
                dest,
                vtable,
                source,
            } => self.emit_dyn_make(*dest, *vtable, *source),
            PhysicalOp::CallableCoerce { dest, source }
            | PhysicalOp::GeneratorCoerce { dest, source } => {
                let value = self.load(*source, "callable.coerce")?;
                self.store(*dest, value)?;
                self.clear_owned(*source)
            }

            PhysicalOp::Const { dest, value } => self.emit_const(*dest, value),
            PhysicalOp::Unary { dest, op, source } => {
                let source_value = self.load(*source, "unary.source")?;
                let value = match op {
                    UnaryOp::Not => {
                        let source = source_value.into_int_value();
                        let logical = self
                            .builder
                            .build_int_compare(
                                IntPredicate::EQ,
                                source,
                                source.get_type().const_zero(),
                                "logical.not",
                            )
                            .llvm_ctx("emit physical logical not")?;
                        let target =
                            llvm_type(self.ctx, &self.storage(*dest)?.layout.repr)?.into_int_type();
                        self.builder
                            .build_int_z_extend(logical, target, "logical.not.widen")
                            .llvm_ctx("widen physical logical not")?
                            .into()
                    }
                    UnaryOp::BitNot => self
                        .builder
                        .build_not(source_value.into_int_value(), "bitnot")
                        .llvm_ctx("emit physical bit not")?
                        .into(),
                    UnaryOp::Negate if source_value.is_float_value() => self
                        .builder
                        .build_float_neg(source_value.into_float_value(), "float.negate")
                        .llvm_ctx("emit IEEE floating negation")?
                        .into(),
                    UnaryOp::Negate | UnaryOp::RawDeref => {
                        return Err(CodegenError::FailClosed(
                            "fallible or raw unary operation reached physical emitter".into(),
                        ));
                    }
                };
                self.store(*dest, value)
            }
            PhysicalOp::Binary { dest, op, lhs, rhs } => self.emit_binary(*dest, *op, *lhs, *rhs),
            PhysicalOp::Cast { dest, source, to } => self.emit_cast(*dest, *source, to),
            PhysicalOp::TupleMake { dest, elements } => {
                let BasicTypeEnum::StructType(tuple_ty) =
                    llvm_type(self.ctx, &self.storage(*dest)?.layout.repr)?
                else {
                    return Err(CodegenError::FailClosed(
                        "physical tuple destination is not an LLVM struct".into(),
                    ));
                };
                let mut aggregate = tuple_ty.get_undef();
                for (index, element) in elements.iter().enumerate() {
                    let field = self.load(*element, "tuple.field")?;
                    let index = u32::try_from(index).map_err(|_| {
                        CodegenError::FailClosed("physical tuple field index exceeds u32".into())
                    })?;
                    aggregate = match self
                        .builder
                        .build_insert_value(aggregate, field, index, "tuple.make")
                        .llvm_ctx("insert physical tuple field")?
                    {
                        inkwell::values::AggregateValueEnum::StructValue(value) => value,
                        inkwell::values::AggregateValueEnum::ArrayValue(_) => {
                            return Err(CodegenError::FailClosed(
                                "physical tuple insertion produced an LLVM array".into(),
                            ));
                        }
                    };
                }
                self.store(*dest, aggregate.into())
            }
            PhysicalOp::TupleGet { dest, tuple, index } => {
                let tuple = self.load(*tuple, "tuple.source")?.into_struct_value();
                let field = self
                    .builder
                    .build_extract_value(tuple, *index, "tuple.get")
                    .llvm_ctx("extract physical tuple field")?;
                self.store(*dest, field)
            }
            PhysicalOp::ArrayMake { dest, fields, glue } => {
                self.emit_array_make(*dest, fields, *glue)
            }
            PhysicalOp::ArrayRepeat { dest, seed, glue } => {
                self.emit_array_repeat(*dest, *seed, *glue)
            }
            PhysicalOp::AggregateMake { dest, fields, .. } => {
                let BasicTypeEnum::StructType(aggregate_ty) =
                    llvm_type(self.ctx, &self.storage(*dest)?.layout.repr)?
                else {
                    return Err(CodegenError::FailClosed(
                        "physical aggregate destination is not an LLVM struct".into(),
                    ));
                };
                let mut aggregate = aggregate_ty.get_undef();
                for (index, field) in fields.iter().enumerate() {
                    let value = self.load(*field, "aggregate.field")?;
                    let index = u32::try_from(index).map_err(|_| {
                        CodegenError::FailClosed(
                            "physical aggregate field index exceeds u32".into(),
                        )
                    })?;
                    aggregate = match self
                        .builder
                        .build_insert_value(aggregate, value, index, "aggregate.make")
                        .llvm_ctx("insert physical aggregate field")?
                    {
                        inkwell::values::AggregateValueEnum::StructValue(value) => value,
                        inkwell::values::AggregateValueEnum::ArrayValue(_) => {
                            return Err(CodegenError::FailClosed(
                                "physical aggregate insertion produced an LLVM array".into(),
                            ));
                        }
                    };
                }
                self.store(*dest, aggregate.into())?;
                for field in fields {
                    self.clear_owned(*field)?;
                }
                Ok(())
            }
            PhysicalOp::AggregateProjectCopy {
                dest,
                aggregate,
                field,
                action,
                ..
            } => {
                let aggregate = self
                    .load(*aggregate, "aggregate.project.source")?
                    .into_struct_value();
                let field_value = self
                    .builder
                    .build_extract_value(aggregate, *field, "aggregate.project.field")
                    .llvm_ctx("extract physical aggregate field for copy")?;
                let value = self.value_emitter().clone_loaded_value(
                    field_value,
                    &self.storage(*dest)?.layout,
                    *action,
                )?;
                self.store(*dest, value)
            }
            PhysicalOp::AggregateProjectBorrow {
                dest,
                aggregate,
                field,
                ..
            } => {
                let aggregate = self
                    .load(*aggregate, "aggregate.borrow.source")?
                    .into_struct_value();
                let value = self
                    .builder
                    .build_extract_value(aggregate, *field, "aggregate.borrow.field")
                    .llvm_ctx("extract verified borrowed aggregate field")?;
                self.store(*dest, value)
            }
            PhysicalOp::AggregateDestructure {
                aggregate, fields, ..
            } => {
                let value = self
                    .load(*aggregate, "aggregate.destructure.source")?
                    .into_struct_value();
                for (index, field) in fields.iter().enumerate() {
                    let index = u32::try_from(index).map_err(|_| {
                        CodegenError::FailClosed(
                            "physical aggregate field index exceeds u32".into(),
                        )
                    })?;
                    let field_value = self
                        .builder
                        .build_extract_value(value, index, "aggregate.destructure.field")
                        .llvm_ctx("extract physical aggregate field")?;
                    self.store(*field, field_value)?;
                }
                self.clear_owned(*aggregate)
            }
            PhysicalOp::VariantMake {
                dest,
                variant,
                fields,
                glue,
            } => self.emit_variant_make(*dest, *variant, fields, *glue),
            PhysicalOp::VariantIs {
                dest,
                source,
                variant,
                glue,
            } => {
                let (tag, _, _) = self.load_variant_tag(*source, *glue)?;
                let matches = self
                    .builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        tag,
                        tag.get_type().const_int(u64::from(*variant), false),
                        "variant.is",
                    )
                    .llvm_ctx("compare physical variant tag")?;
                let bool_ty =
                    llvm_type(self.ctx, &self.storage(*dest)?.layout.repr)?.into_int_type();
                let value = if matches.get_type() == bool_ty {
                    matches
                } else {
                    self.builder
                        .build_int_z_extend(matches, bool_ty, "bool.widen")
                        .llvm_ctx("widen physical variant test")?
                };
                self.store(*dest, value.into())
            }
            PhysicalOp::VariantProjectCopy {
                dest,
                source,
                variant,
                field,
                glue,
                action,
            } => {
                let payload = self.variant_field_payload(*source, *variant, *glue)?;
                let field_value = self
                    .builder
                    .build_extract_value(payload, *field, "variant.project.field")
                    .llvm_ctx("extract physical variant field for copy")?;
                let value = self.value_emitter().clone_loaded_value(
                    field_value,
                    &self.storage(*dest)?.layout,
                    *action,
                )?;
                self.store(*dest, value)
            }
            PhysicalOp::VariantProjectBorrow {
                dest,
                source,
                variant,
                field,
                glue,
            } => {
                let payload = self.variant_field_payload(*source, *variant, *glue)?;
                let value = self
                    .builder
                    .build_extract_value(payload, *field, "variant.borrow.field")
                    .llvm_ctx("extract verified borrowed variant field")?;
                self.store(*dest, value)
            }
            PhysicalOp::VariantDestructure {
                source,
                variant,
                fields,
                glue,
            } => {
                let (payload, layout, object) =
                    self.load_variant_payload(*source, *variant, *glue)?;
                if let Some(payload) = payload {
                    for (index, field) in fields.iter().enumerate() {
                        let index = u32::try_from(index).map_err(|_| {
                            CodegenError::FailClosed("variant field index exceeds u32".into())
                        })?;
                        let value = self
                            .builder
                            .build_extract_value(payload, index, "variant.destructure.field")
                            .llvm_ctx("extract physical variant payload field")?;
                        self.store(*field, value)?;
                    }
                }
                if layout.is_indirect {
                    self.value_emitter().free_variant_node(object, layout)?;
                }
                self.clear_owned(*source)
            }
            PhysicalOp::Transfer { dest, source } => {
                let value = self.load(*source, "transfer")?;
                self.store(*dest, value)?;
                if dest != source {
                    self.clear_owned(*source)?;
                }
                Ok(())
            }
            PhysicalOp::Clone {
                dest,
                source,
                action,
            } => {
                let value = self.clone_value(*source, *action)?;
                self.store(*dest, value)
            }
            PhysicalOp::Destroy {
                source,
                action,
                cleanup,
            } => self.destroy_value(*source, *action, cleanup),
            PhysicalOp::Borrow { dest, source } => {
                let value = self.load(*source, "borrow")?;
                self.store(*dest, value)
            }
            PhysicalOp::EndBorrow { .. } => Ok(()),
            PhysicalOp::StorageLive { storage } => self.set_place_initialized(*storage, false),
            PhysicalOp::Assign {
                dest,
                source,
                destroy_old,
                cleanup,
            } => {
                if let Some(action) = destroy_old {
                    self.destroy_value(*dest, *action, cleanup)?;
                }
                let value = self.load(*source, "assign")?;
                self.store(*dest, value)?;
                self.clear_owned(*source)
            }
            PhysicalOp::StorageDead {
                storage,
                destroy,
                cleanup,
            } => {
                if self.destroy_certified_contents(*storage, cleanup)? {
                    return Ok(());
                }
                // A deferred actor seat (D447) has no local partition: the
                // verifier proved it initialized here, so release directly.
                if matches!(
                    self.storage(*storage)?.origin,
                    hew_mir::physical::StorageOrigin::ActorState {
                        initialized: false,
                        ..
                    }
                ) {
                    if let Some(action) = destroy {
                        let value = self.load(*storage, "seat.release")?;
                        self.release_loaded(value, &self.storage(*storage)?.layout, *action)?;
                    }
                    return Ok(());
                }
                Err(CodegenError::FailClosed(
                    "local lifetime lacks a verified content partition".into(),
                ))
            }
        }
    }

    fn emit_variant_make(
        &self,
        dest: StorageId,
        variant: u32,
        fields: &[StorageId],
        glue_id: PhysicalVariantId,
    ) -> CodegenResult<()> {
        let values = fields
            .iter()
            .map(|field| self.load(*field, "variant.make.field"))
            .collect::<CodegenResult<Vec<_>>>()?;
        self.write_variant_value(self.slots[dest.0 as usize], variant, &values, glue_id)?;
        for field in fields {
            self.clear_owned(*field)?;
        }
        Ok(())
    }

    pub(super) fn write_variant_value(
        &self,
        destination: PointerValue<'ctx>,
        variant: u32,
        fields: &[BasicValueEnum<'ctx>],
        glue_id: PhysicalVariantId,
    ) -> CodegenResult<()> {
        self.value_emitter()
            .write_variant_value(destination, variant, fields, glue_id)
    }

    /// Read the tag of one enum value with its layout and tag-and-payload object.
    pub(super) fn load_variant_tag(
        &self,
        source: StorageId,
        glue_id: PhysicalVariantId,
    ) -> CodegenResult<(IntValue<'ctx>, &PhysicalVariantLayout, PointerValue<'ctx>)> {
        let values = self.value_emitter();
        let glue = values.variant_glue(glue_id)?;
        let layout = values.variant_layout(&glue.ty)?;
        let object = values.variant_object_ptr(self.slots[source.0 as usize], layout)?;
        let tag = values.load_variant_tag(object, layout)?;
        Ok((tag, layout, object))
    }

    /// Load one tested case's payload, absent for a payload-free case, with
    /// the object it came from. A different runtime tag is corrupt
    /// representation and traps, exactly like an unmatched switch arm.
    fn load_variant_payload(
        &self,
        source: StorageId,
        variant: u32,
        glue_id: PhysicalVariantId,
    ) -> CodegenResult<(
        Option<StructValue<'ctx>>,
        &PhysicalVariantLayout,
        PointerValue<'ctx>,
    )> {
        let (tag, layout, object) = self.load_variant_tag(source, glue_id)?;
        let matches = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                tag,
                tag.get_type().const_int(u64::from(variant), false),
                "variant.project.tested",
            )
            .llvm_ctx("compare physical variant tag")?;
        let valid = self.ctx.append_basic_block(self.value, "variant.project");
        let invalid = self.ctx.append_basic_block(self.value, "variant.invalid");
        self.builder
            .build_conditional_branch(matches, valid, invalid)
            .llvm_ctx("branch on physical variant tag")?;
        self.builder.position_at_end(invalid);
        self.value_emitter().emit_invalid_variant_tag()?;
        self.builder.position_at_end(valid);
        let payload_ty =
            llvm_type(self.ctx, &layout.variants[variant as usize].repr)?.into_struct_type();
        if payload_ty.count_fields() == 0 {
            return Ok((None, layout, object));
        }
        let payload_ptr = self.value_emitter().variant_payload_ptr(object, layout)?;
        let payload = self
            .builder
            .build_load(payload_ty, payload_ptr, "variant.project.payload")
            .llvm_ctx("load physical variant payload")?
            .into_struct_value();
        Ok((Some(payload), layout, object))
    }

    /// The payload a field projection reads; a projected case always has one.
    fn variant_field_payload(
        &self,
        source: StorageId,
        variant: u32,
        glue_id: PhysicalVariantId,
    ) -> CodegenResult<StructValue<'ctx>> {
        self.load_variant_payload(source, variant, glue_id)?
            .0
            .ok_or_else(|| {
                CodegenError::FailClosed("variant projection reads a payload-free case".into())
            })
    }

    pub(super) fn emit_variant_switch(
        &self,
        scrutinee: StorageId,
        glue_id: PhysicalVariantId,
        arms: &[PhysicalVariantArm],
    ) -> CodegenResult<()> {
        let (tag, layout, object) = self.load_variant_tag(scrutinee, glue_id)?;
        let invalid = self.ctx.append_basic_block(self.value, "variant.invalid");
        let arm_blocks = arms
            .iter()
            .map(|arm| {
                (
                    tag.get_type().const_int(u64::from(arm.variant), false),
                    self.ctx
                        .append_basic_block(self.value, &format!("variant.case.{}", arm.variant)),
                )
            })
            .collect::<Vec<_>>();
        self.builder
            .build_switch(tag, invalid, &arm_blocks)
            .llvm_ctx("emit physical variant switch")?;
        for (arm, (_, arm_block)) in arms.iter().zip(&arm_blocks) {
            self.builder.position_at_end(*arm_block);
            let payload_layout = &layout.variants[arm.variant as usize];
            if !arm.fields.is_empty() {
                let payload_ty = llvm_type(self.ctx, &payload_layout.repr)?.into_struct_type();
                let payload_ptr = self.value_emitter().variant_payload_ptr(object, layout)?;
                let payload = self
                    .builder
                    .build_load(payload_ty, payload_ptr, "variant.switch.payload")
                    .llvm_ctx("load physical variant payload")?
                    .into_struct_value();
                for (index, field) in arm.fields.iter().enumerate() {
                    let index = u32::try_from(index).map_err(|_| {
                        CodegenError::FailClosed("variant field index exceeds u32".into())
                    })?;
                    let value = self
                        .builder
                        .build_extract_value(payload, index, "variant.switch.field")
                        .llvm_ctx("extract physical variant payload field")?;
                    self.store(*field, value)?;
                }
            }
            if layout.is_indirect {
                self.value_emitter().free_variant_node(object, layout)?;
            }
            self.clear_owned(scrutinee)?;
            self.emit_edge(&arm.target)?;
        }
        self.builder.position_at_end(invalid);
        self.value_emitter().emit_invalid_variant_tag()
    }

    fn emit_const(&self, dest: StorageId, value: &PhysicalConst) -> CodegenResult<()> {
        let llvm_ty = llvm_type(self.ctx, &self.storage(dest)?.layout.repr)?;
        match value {
            PhysicalConst::ActorIngressAdapter(adapter) => {
                let function = self
                    .llvm
                    .get_function(&actor::ingress_symbol(*adapter))
                    .ok_or_else(|| {
                        CodegenError::FailClosed("missing actor ingress adapter".into())
                    })?;
                self.store(dest, function.as_global_value().as_pointer_value().into())
            }
            // Physical MIR already derived the exact destination-width bit
            // pattern, so the backend emits it verbatim: no sign inference, no
            // widening decision.
            PhysicalConst::IntegerBits(bits) => {
                self.store(dest, llvm_ty.into_int_type().const_int(*bits, false).into())
            }
            // Duration keeps its own signed `i64` semantic type.
            PhysicalConst::Duration(value) => self.store(
                dest,
                llvm_ty
                    .into_int_type()
                    .const_int(*value as u64, true)
                    .into(),
            ),
            PhysicalConst::Bool(value) => self.store(
                dest,
                llvm_ty
                    .into_int_type()
                    .const_int(u64::from(*value), false)
                    .into(),
            ),
            PhysicalConst::Float(value) => {
                self.store(dest, llvm_ty.into_float_type().const_float(*value).into())
            }
            PhysicalConst::Char(value) => self.store(
                dest,
                llvm_ty
                    .into_int_type()
                    .const_int(u64::from(u32::from(*value)), false)
                    .into(),
            ),
            PhysicalConst::Unit => self.store(dest, llvm_ty.const_zero()),
            PhysicalConst::String(id) => {
                let bytes = self.module.string_literals.get(id).ok_or_else(|| {
                    CodegenError::FailClosed(format!("missing physical string literal {}", id.0))
                })?;
                self.emit_literal(dest, bytes.as_bytes(), "hew_string_literal_new")
            }
            PhysicalConst::Bytes(id) => {
                let bytes = self.module.bytes_literals.get(id).ok_or_else(|| {
                    CodegenError::FailClosed(format!("missing physical bytes literal {}", id.0))
                })?;
                self.emit_literal(dest, bytes, "hew_bytes_literal_new")
            }
        }
    }

    fn emit_literal(&self, dest: StorageId, bytes: &[u8], symbol: &str) -> CodegenResult<()> {
        let len = u32::try_from(bytes.len()).map_err(|_| {
            CodegenError::FailClosed(format!("{symbol} literal exceeds the u32 runtime ABI"))
        })?;
        let data = self.ctx.const_string(bytes, false);
        let global = self
            .llvm
            .add_global(data.get_type(), None, "physical.literal");
        global.set_linkage(Linkage::Private);
        global.set_initializer(&data);
        global.set_constant(true);
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let function = get_or_declare_external(
            self.llvm,
            symbol,
            self.ctx
                .void_type()
                .fn_type(&[ptr.into(), self.ctx.i32_type().into(), ptr.into()], false),
        )?;
        self.builder
            .build_call(
                function,
                &[
                    global.as_pointer_value().into(),
                    self.ctx.i32_type().const_int(u64::from(len), false).into(),
                    self.slots[dest.0 as usize].into(),
                ],
                "literal.new",
            )
            .llvm_ctx("create owned physical literal")?;
        Ok(())
    }

    fn emit_binary(
        &self,
        dest: StorageId,
        op: BinaryOp,
        lhs: StorageId,
        rhs: StorageId,
    ) -> CodegenResult<()> {
        let left = self.load(lhs, "binary.left")?;
        let right = self.load(rhs, "binary.right")?;
        let ty = &self.storage(lhs)?.ty;
        let mut value: BasicValueEnum<'ctx> = match (left, right) {
            (BasicValueEnum::IntValue(left), BasicValueEnum::IntValue(right)) => {
                emit_integer_binary(&self.builder, op, left, right, is_signed(ty))?.into()
            }
            (BasicValueEnum::FloatValue(left), BasicValueEnum::FloatValue(right)) => {
                emit_float_binary(&self.builder, op, left, right)?
            }
            (BasicValueEnum::StructValue(left), BasicValueEnum::StructValue(right))
                if ty.is_builtin(hew_types::BuiltinType::ChildRef) && op == BinaryOp::Equal =>
            {
                let mut equal = self.ctx.bool_type().const_int(1, false);
                for index in 0..2 {
                    let left = self
                        .builder
                        .build_extract_value(left, index, "identity.left")
                        .llvm_ctx("read supervised role identity")?
                        .into_int_value();
                    let right = self
                        .builder
                        .build_extract_value(right, index, "identity.right")
                        .llvm_ctx("read supervised role identity")?
                        .into_int_value();
                    let component = self
                        .builder
                        .build_int_compare(IntPredicate::EQ, left, right, "identity.component")
                        .llvm_ctx("compare supervised role identity")?;
                    equal = self
                        .builder
                        .build_and(equal, component, "identity.equal")
                        .llvm_ctx("combine supervised role identity")?;
                }
                equal.into()
            }
            _ => {
                return Err(CodegenError::FailClosed(
                    "physical binary operands have unsupported carriers".into(),
                ));
            }
        };
        if self.storage(dest)?.ty == ResolvedTy::Bool {
            let int = value.into_int_value();
            let bool_ty = llvm_type(self.ctx, &self.storage(dest)?.layout.repr)?.into_int_type();
            if int.get_type() != bool_ty {
                value = self
                    .builder
                    .build_int_z_extend(int, bool_ty, "bool.widen")
                    .llvm_ctx("widen physical boolean result")?
                    .into();
            }
        }
        self.store(dest, value)
    }

    fn emit_cast(&self, dest: StorageId, source: StorageId, to: &ResolvedTy) -> CodegenResult<()> {
        let value = self.load(source, "cast.source")?;
        let target = llvm_type(self.ctx, &self.storage(dest)?.layout.repr)?;
        let source_ty = &self.storage(source)?.ty;
        let cast = match (value, target) {
            (BasicValueEnum::IntValue(value), BasicTypeEnum::IntType(target)) => self
                .builder
                .build_int_cast_sign_flag(value, target, is_signed(source_ty), "int.cast")
                .llvm_ctx("emit physical integer cast")?
                .into(),
            (BasicValueEnum::FloatValue(value), BasicTypeEnum::FloatType(target)) => self
                .builder
                .build_float_cast(value, target, "float.cast")
                .llvm_ctx("emit physical float cast")?
                .into(),
            (BasicValueEnum::IntValue(value), BasicTypeEnum::FloatType(target)) => {
                if is_signed(source_ty) {
                    self.builder
                        .build_signed_int_to_float(value, target, "signed.to.float")
                        .llvm_ctx("emit signed integer-to-float cast")?
                        .into()
                } else {
                    self.builder
                        .build_unsigned_int_to_float(value, target, "unsigned.to.float")
                        .llvm_ctx("emit unsigned integer-to-float cast")?
                        .into()
                }
            }
            (BasicValueEnum::FloatValue(value), BasicTypeEnum::IntType(target)) => {
                // Plain `fptosi`/`fptoui` are LLVM poison for out-of-range and
                // non-finite inputs. The spec guarantees saturating semantics
                // (MAX/MIN on overflow, 0 on NaN) for every `as` float-to-int
                // cast, so the saturating intrinsics are the only correct
                // lowering here (see HEW-SPEC-2026.md's float-to-integer
                // cast table).
                let name = if is_signed(to) {
                    "llvm.fptosi.sat"
                } else {
                    "llvm.fptoui.sat"
                };
                let declaration = Intrinsic::find(name)
                    .and_then(|intrinsic| {
                        intrinsic
                            .get_declaration(self.llvm, &[target.into(), value.get_type().into()])
                    })
                    .ok_or_else(|| {
                        CodegenError::FailClosed(format!(
                            "LLVM cast intrinsic `{name}` is unavailable"
                        ))
                    })?;
                self.runtime_call_value(declaration, &[value.into()], "float.to.int.sat")?
            }
            _ => {
                return Err(CodegenError::FailClosed(
                    "physical cast has unsupported carriers".into(),
                ));
            }
        };
        self.store(dest, cast)
    }

    pub(super) fn clone_value(
        &self,
        source: StorageId,
        action: CloneAction,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let value = self.load(source, "clone.source")?;
        self.value_emitter()
            .clone_loaded_value(value, &self.storage(source)?.layout, action)
    }

    fn destroy_value(
        &self,
        source: StorageId,
        action: DestroyAction,
        cleanup: &PhysicalCleanup,
    ) -> CodegenResult<()> {
        if self.destroy_certified_contents(source, cleanup)? {
            return Ok(());
        }
        let value = self.load(source, "destroy.source")?;
        self.clear_owned(source)?;
        self.release_loaded(value, &self.storage(source)?.layout, action)
    }

    /// Release an operand this emitter still owns on an operation's own
    /// failure path. There is no SIR cleanup site here, so every leaf that
    /// holds contents at run time is released.
    pub(super) fn destroy_owned_operand(
        &self,
        source: StorageId,
        action: DestroyAction,
    ) -> CodegenResult<()> {
        if self.destroy_initialized_contents(source)? {
            return Ok(());
        }
        let value = self.load(source, "destroy.source")?;
        self.clear_owned(source)?;
        self.release_loaded(value, &self.storage(source)?.layout, action)
    }
}
