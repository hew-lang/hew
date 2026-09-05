//! Key callbacks execute the exact physical capability selections. They borrow
//! value slots and publish a result only after every selected operation succeeds.

use hew_mir::physical::{PhysicalValueCapability, PhysicalValueMethod};
use hew_types::ValueCapability;
use inkwell::types::IntType;

use super::*;

type CallbackTable<'ctx> = BTreeMap<(ResolvedTy, ValueCapability), FunctionValue<'ctx>>;

const FNV_OFFSET: u64 = 0xcbf2_9ce4_8422_2325;
const FNV_PRIME: u64 = 0x100_0000_01b3;

impl<'ctx> ModuleEmitter<'ctx, '_> {
    pub(super) fn emit_collection_key_descriptors(&self) -> CodegenResult<()> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let mut callbacks = BTreeMap::new();
        // Declare the complete selected graph before emitting any body. Symbols
        // encode table order, never a type's display name or a reconstructed plan.
        for (index, (key, _)) in self.module.value_capabilities.iter().enumerate() {
            let arity = match key.1 {
                ValueCapability::Hash => 3,
                ValueCapability::Eq => 4,
            };
            let function = self.llvm.add_function(
                &format!("__hew_key_callback_{index}"),
                self.ctx
                    .i32_type()
                    .fn_type(&vec![pointer.into(); arity], false),
                Some(Linkage::Internal),
            );
            callbacks.insert(key.clone(), function);
        }
        for ((ty, capability), selection) in &self.module.value_capabilities {
            let function = callbacks[&(ty.clone(), *capability)];
            KeyEmitter::new(self, &callbacks, function, *capability)?.emit(ty, selection)?;
        }
        for function in &self.module.functions {
            for block in &function.blocks {
                let PhysicalTerminator::RuntimeCall { action, .. } = &block.terminator else {
                    continue;
                };
                let (name, recipe) = match action {
                    PhysicalRuntimeAction::Map {
                        operation: PhysicalMapOp::New,
                        glue,
                    } => {
                        let recipe = self
                            .module
                            .map_glue
                            .get(glue.0 as usize)
                            .filter(|recipe| recipe.id == *glue)
                            .ok_or_else(|| {
                                CodegenError::FailClosed("key descriptor has no map recipe".into())
                            })?;
                        (map_key_descriptor_symbol(*glue), &recipe.key)
                    }
                    PhysicalRuntimeAction::Set {
                        operation: PhysicalSetOp::New,
                        glue,
                    } => {
                        let recipe = self
                            .module
                            .set_glue
                            .get(glue.0 as usize)
                            .filter(|recipe| recipe.id == *glue)
                            .ok_or_else(|| {
                                CodegenError::FailClosed("key descriptor has no set recipe".into())
                            })?;
                        (set_key_descriptor_symbol(*glue), &recipe.element)
                    }
                    _ => continue,
                };
                if self.llvm.get_global(&name).is_none() {
                    self.emit_key_descriptor(&name, recipe, &callbacks)?;
                }
            }
        }
        Ok(())
    }

    fn emit_key_descriptor(
        &self,
        name: &str,
        recipe: &PhysicalValueRecipe,
        callbacks: &CallbackTable<'ctx>,
    ) -> CodegenResult<()> {
        let hash = callbacks
            .get(&(recipe.ty.clone(), ValueCapability::Hash))
            .ok_or_else(|| {
                CodegenError::FailClosed("demanded collection key lacks selected Hash".into())
            })?;
        let eq = callbacks
            .get(&(recipe.ty.clone(), ValueCapability::Eq))
            .ok_or_else(|| {
                CodegenError::FailClosed("demanded collection key lacks selected Eq".into())
            })?;
        let value = self.value_descriptor(name, recipe)?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let descriptor_ty = self.ctx.struct_type(
            &[value.get_type().into(), pointer.into(), pointer.into()],
            false,
        );
        let descriptor = descriptor_ty.const_named_struct(&[
            value.into(),
            hash.as_global_value().as_pointer_value().into(),
            eq.as_global_value().as_pointer_value().into(),
        ]);
        let global = self.llvm.add_global(descriptor_ty, None, name);
        global.set_linkage(Linkage::Internal);
        global.set_constant(true);
        global.set_initializer(&descriptor);
        Ok(())
    }
}

struct KeyEmitter<'a, 'ctx, 'm> {
    parent: &'a ModuleEmitter<'ctx, 'm>,
    callbacks: &'a CallbackTable<'ctx>,
    builder: Builder<'ctx>,
    function: FunctionValue<'ctx>,
    capability: ValueCapability,
    out: PointerValue<'ctx>,
    fault: PointerValue<'ctx>,
}

impl<'a, 'ctx, 'm> KeyEmitter<'a, 'ctx, 'm> {
    fn new(
        parent: &'a ModuleEmitter<'ctx, 'm>,
        callbacks: &'a CallbackTable<'ctx>,
        function: FunctionValue<'ctx>,
        capability: ValueCapability,
    ) -> CodegenResult<Self> {
        let output_index = match capability {
            ValueCapability::Hash => 1,
            ValueCapability::Eq => 2,
        };
        let parameter = |index| {
            function
                .get_nth_param(index)
                .map(BasicValueEnum::into_pointer_value)
                .ok_or_else(|| CodegenError::FailClosed("key callback lacks ABI parameter".into()))
        };
        let entry = parent.ctx.append_basic_block(function, "entry");
        let body = parent.ctx.append_basic_block(function, "body");
        let builder = parent.ctx.create_builder();
        builder.position_at_end(entry);
        builder
            .build_unconditional_branch(body)
            .llvm_ctx("enter key callback")?;
        builder.position_at_end(body);
        Ok(Self {
            parent,
            callbacks,
            builder,
            function,
            capability,
            out: parameter(output_index)?,
            fault: parameter(output_index + 1)?,
        })
    }

    fn emit(self, ty: &ResolvedTy, selection: &PhysicalValueCapability) -> CodegenResult<()> {
        let lhs = self.parameter(0)?;
        let rhs = if self.capability == ValueCapability::Eq {
            Some(self.parameter(1)?)
        } else {
            None
        };
        match selection.method {
            PhysicalValueMethod::User(id) => self.user(id, lhs, rhs),
            PhysicalValueMethod::Scalar => {
                let left = self.scalar_bits(ty, lhs)?;
                let result = if let Some(rhs) = rhs {
                    self.equal(left, self.scalar_bits(ty, rhs)?)?
                } else {
                    left
                };
                self.finish(result)
            }
            PhysicalValueMethod::String => self.string(lhs, rhs),
            PhysicalValueMethod::Bytes => self.bytes(lhs, rhs),
            PhysicalValueMethod::Aggregate(id) => {
                let glue = self.values().aggregate_glue(id)?;
                let layout = self.layout(&glue.ty)?;
                self.fields(&layout.repr, &glue.fields, lhs, rhs)
            }
            PhysicalValueMethod::Variant(id) => {
                let rhs = self.require_eq(rhs, "variant")?;
                self.variant(id, lhs, rhs)
            }
            PhysicalValueMethod::Vector(id) => {
                let rhs = self.require_eq(rhs, "vector")?;
                let glue = self.values().vector_glue(id)?;
                self.vector(&glue.element.ty, lhs, rhs)
            }
            PhysicalValueMethod::Map(_) | PhysicalValueMethod::Set(_) => {
                Err(CodegenError::FailClosed(
                    "structural Map/Set key capabilities are outside checker admission".into(),
                ))
            }
        }
    }

    fn require_eq(
        &self,
        rhs: Option<PointerValue<'ctx>>,
        kind: &str,
    ) -> CodegenResult<PointerValue<'ctx>> {
        rhs.ok_or_else(|| {
            CodegenError::FailClosed(format!("derived {kind} Hash is outside checker admission"))
        })
    }

    fn values(&self) -> ValueEmitter<'_, 'ctx> {
        ValueEmitter {
            module: self.parent.module,
            ctx: self.parent.ctx,
            llvm: &self.parent.llvm,
            builder: &self.builder,
            value: self.function,
        }
    }

    fn parameter(&self, index: u32) -> CodegenResult<PointerValue<'ctx>> {
        self.function
            .get_nth_param(index)
            .map(BasicValueEnum::into_pointer_value)
            .ok_or_else(|| CodegenError::FailClosed("key callback lacks input parameter".into()))
    }

    fn layout(&self, ty: &ResolvedTy) -> CodegenResult<&'m PhysicalLayout> {
        self.parent.module.target.layout(ty).ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "selected key type `{}` lacks a physical layout",
                ty.user_facing()
            ))
        })
    }

    fn load(
        &self,
        ty: BasicTypeEnum<'ctx>,
        source: PointerValue<'ctx>,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        self.builder
            .build_load(ty, source, "key.load")
            .llvm_ctx("load borrowed key slot")
    }

    fn handle(&self, source: PointerValue<'ctx>) -> CodegenResult<PointerValue<'ctx>> {
        Ok(self
            .load(
                self.parent.ctx.ptr_type(AddressSpace::default()).into(),
                source,
            )?
            .into_pointer_value())
    }

    fn block(&self, name: &str) -> BasicBlock<'ctx> {
        self.parent.ctx.append_basic_block(self.function, name)
    }

    fn jump(&self, dest: BasicBlock<'ctx>) -> CodegenResult<()> {
        self.builder
            .build_unconditional_branch(dest)
            .llvm_ctx("advance key callback")?;
        Ok(())
    }

    fn branch(
        &self,
        test: IntValue<'ctx>,
        yes: BasicBlock<'ctx>,
        no: BasicBlock<'ctx>,
    ) -> CodegenResult<()> {
        self.builder
            .build_conditional_branch(test, yes, no)
            .llvm_ctx("branch in key callback")?;
        Ok(())
    }

    fn equal(&self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> CodegenResult<IntValue<'ctx>> {
        self.builder
            .build_int_compare(IntPredicate::EQ, lhs, rhs, "key.equal")
            .llvm_ctx("compare key values")
    }

    fn truth(&self, value: IntValue<'ctx>) -> CodegenResult<IntValue<'ctx>> {
        self.builder
            .build_int_compare(
                IntPredicate::NE,
                value,
                value.get_type().const_zero(),
                "key.truth",
            )
            .llvm_ctx("normalize key equality")
    }

    fn call(
        &self,
        function: FunctionValue<'ctx>,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        self.builder
            .build_call(function, args, "key.call")
            .llvm_ctx("call key operation")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("key operation returned void".into()))
    }

    fn runtime(
        &self,
        symbol: &str,
        ty: FunctionType<'ctx>,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        self.call(
            get_or_declare_external(&self.parent.llvm, symbol, ty)?,
            args,
        )
    }

    fn finish(&self, value: IntValue<'ctx>) -> CodegenResult<()> {
        let value = if self.capability == ValueCapability::Eq {
            self.builder
                .build_int_z_extend(self.truth(value)?, self.parent.ctx.i8_type(), "key.bool")
                .llvm_ctx("store C boolean key result")?
        } else {
            value
        };
        self.builder
            .build_store(self.out, value)
            .llvm_ctx("publish successful key result")?;
        self.builder
            .build_store(
                self.fault,
                self.parent
                    .ctx
                    .ptr_type(AddressSpace::default())
                    .const_null(),
            )
            .llvm_ctx("clear successful key fault")?;
        self.builder
            .build_return(Some(&self.parent.ctx.i32_type().const_zero()))
            .llvm_ctx("return key success")?;
        Ok(())
    }

    fn status(&self, status: IntValue<'ctx>) -> CodegenResult<()> {
        let success = self.block("key.success");
        let failed = self.block("key.failed");
        self.branch(
            self.equal(status, status.get_type().const_zero())?,
            success,
            failed,
        )?;
        self.builder.position_at_end(failed);
        // The callee wrote the opaque fault directly. Preserve its exact status,
        // do not read its result storage, and leave our caller's output untouched.
        self.builder
            .build_return(Some(&status))
            .llvm_ctx("propagate selected key fault")?;
        self.builder.position_at_end(success);
        Ok(())
    }

    fn continue_equal(&self, equal: IntValue<'ctx>) -> CodegenResult<()> {
        let next = self.block("key.equal.next");
        let unequal = self.block("key.unequal");
        self.branch(self.truth(equal)?, next, unequal)?;
        self.builder.position_at_end(unequal);
        self.finish(self.parent.ctx.bool_type().const_zero())?;
        self.builder.position_at_end(next);
        Ok(())
    }

    fn component(
        &self,
        ty: &ResolvedTy,
        lhs: PointerValue<'ctx>,
        rhs: Option<PointerValue<'ctx>>,
    ) -> CodegenResult<IntValue<'ctx>> {
        let selected = self
            .callbacks
            .get(&(ty.clone(), self.capability))
            .copied()
            .ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "selected {:?} component `{}` has no callback",
                    self.capability,
                    ty.user_facing()
                ))
            })?;
        let result_ty = self.result_type();
        let output = self
            .values()
            .entry_scratch(result_ty.into(), "key.component.result")?;
        let mut args = vec![lhs.into()];
        if let Some(rhs) = rhs {
            args.push(rhs.into());
        }
        args.push(output.into());
        args.push(self.fault.into());
        let status = self.call(selected, &args)?.into_int_value();
        self.status(status)?;
        Ok(self.load(result_ty.into(), output)?.into_int_value())
    }

    fn result_type(&self) -> IntType<'ctx> {
        match self.capability {
            ValueCapability::Hash => self.parent.ctx.i64_type(),
            ValueCapability::Eq => self.parent.ctx.i8_type(),
        }
    }

    fn user(
        &self,
        id: CallableId,
        lhs: PointerValue<'ctx>,
        rhs: Option<PointerValue<'ctx>>,
    ) -> CodegenResult<()> {
        let callable = callable(self.parent.module, id)?;
        let selected = self.parent.functions.get(&id).copied().ok_or_else(|| {
            CodegenError::FailClosed("selected user key method has no LLVM declaration".into())
        })?;
        let inputs = if let Some(rhs) = rhs {
            vec![lhs, rhs]
        } else {
            vec![lhs]
        };
        if inputs.len() != callable.params.len() {
            return Err(CodegenError::FailClosed(
                "selected user key method has incorrect arity".into(),
            ));
        }
        let mut args = inputs
            .into_iter()
            .zip(&callable.params)
            .map(|(slot, param)| match param.carrier {
                ParamCarrier::Indirect => Ok(slot.into()),
                ParamCarrier::Direct => self
                    .load(llvm_type(self.parent.ctx, &param.layout.repr)?, slot)
                    .map(Into::into),
            })
            .collect::<CodegenResult<Vec<BasicMetadataValueEnum<'ctx>>>>()?;
        let result = callable.return_layout.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("selected user key method has no result layout".into())
        })?;
        let result_ty = llvm_type(self.parent.ctx, &result.repr)?;
        let output = self.values().entry_scratch(result_ty, "key.user.result")?;
        args.push(output.into());
        args.push(self.fault.into());
        self.status(self.call(selected, &args)?.into_int_value())?;
        self.finish(self.load(result_ty, output)?.into_int_value())
    }

    fn scalar_bits(
        &self,
        ty: &ResolvedTy,
        slot: PointerValue<'ctx>,
    ) -> CodegenResult<IntValue<'ctx>> {
        let ctx = self.parent.ctx;
        if *ty == ResolvedTy::Unit {
            return Ok(ctx.i64_type().const_zero());
        }
        let layout = self.layout(ty)?;
        let loaded = self.load(llvm_type(ctx, &layout.repr)?, slot)?;
        let bits = match loaded {
            BasicValueEnum::IntValue(value) if *ty == ResolvedTy::Bool => self.truth(value)?,
            BasicValueEnum::IntValue(value) => value,
            BasicValueEnum::FloatValue(value) => {
                // Key equality is total and bitwise, including signed zero and
                // NaN payloads. Hash consumes those same bits at every depth.
                let int_ty = match layout.repr {
                    PhysicalRepr::Float { bits: 32 } => ctx.i32_type(),
                    PhysicalRepr::Float { bits: 64 } => ctx.i64_type(),
                    _ => {
                        return Err(CodegenError::FailClosed(
                            "key scalar has unsupported float layout".into(),
                        ))
                    }
                };
                self.builder
                    .build_bit_cast(value, int_ty, "key.float.bits")
                    .llvm_ctx("read exact floating key bits")?
                    .into_int_value()
            }
            _ => {
                return Err(CodegenError::FailClosed(
                    "selected scalar key has a non-scalar layout".into(),
                ))
            }
        };
        self.builder
            .build_int_z_extend_or_bit_cast(bits, ctx.i64_type(), "key.bits")
            .llvm_ctx("widen key scalar bits")
    }

    fn string(
        &self,
        lhs: PointerValue<'ctx>,
        rhs: Option<PointerValue<'ctx>>,
    ) -> CodegenResult<()> {
        let ctx = self.parent.ctx;
        let ptr = ctx.ptr_type(AddressSpace::default());
        let lhs = self.handle(lhs)?;
        let value = if let Some(rhs) = rhs {
            self.runtime(
                "hew_string_equals",
                ctx.i32_type().fn_type(&[ptr.into(), ptr.into()], false),
                &[lhs.into(), self.handle(rhs)?.into()],
            )?
        } else {
            self.runtime(
                "hew_string_hash_fnv1a",
                ctx.i64_type().fn_type(&[ptr.into()], false),
                &[lhs.into()],
            )?
        };
        self.finish(value.into_int_value())
    }

    fn bytes(&self, lhs: PointerValue<'ctx>, rhs: Option<PointerValue<'ctx>>) -> CodegenResult<()> {
        let ctx = self.parent.ctx;
        let bytes_ty = llvm_type(ctx, &self.layout(&ResolvedTy::Bytes)?.repr)?.into_struct_type();
        let left = self.load(bytes_ty.into(), lhs)?.into_struct_value();
        let extract = |value, index| {
            self.builder
                .build_extract_value(value, index, "key.bytes.part")
                .llvm_ctx("read active byte region")
        };
        let data = extract(left, 0)?.into_pointer_value();
        let offset = extract(left, 1)?.into_int_value();
        let length = extract(left, 2)?.into_int_value();
        if let Some(rhs) = rhs {
            let right = self.load(bytes_ty.into(), rhs)?.into_struct_value();
            let ptr = ctx.ptr_type(AddressSpace::default());
            let word = ctx.i32_type();
            let value = self.runtime(
                "hew_bytes_eq",
                ctx.bool_type().fn_type(
                    &[
                        ptr.into(),
                        word.into(),
                        word.into(),
                        ptr.into(),
                        word.into(),
                        word.into(),
                    ],
                    false,
                ),
                &[
                    data.into(),
                    offset.into(),
                    length.into(),
                    extract(right, 0)?.into(),
                    extract(right, 1)?.into(),
                    extract(right, 2)?.into(),
                ],
            )?;
            return self.finish(value.into_int_value());
        }
        // Widen before adding: offset+index is an address, not wrapping u32
        // owner metadata. Empty regions never form or dereference a data pointer.
        let target = TargetData::create(&self.parent.module.target.data_layout);
        let size = ctx.ptr_sized_int_type(&target, None);
        let offset = self
            .builder
            .build_int_z_extend_or_bit_cast(offset, size, "key.bytes.offset")
            .llvm_ctx("widen bytes offset")?;
        let length = self
            .builder
            .build_int_z_extend_or_bit_cast(length, size, "key.bytes.length")
            .llvm_ctx("widen bytes length")?;
        let index = self
            .values()
            .entry_scratch(size.into(), "key.bytes.index")?;
        let hash = self
            .values()
            .entry_scratch(ctx.i64_type().into(), "key.bytes.hash")?;
        self.builder
            .build_store(index, size.const_zero())
            .llvm_ctx("initialize byte cursor")?;
        self.builder
            .build_store(hash, ctx.i64_type().const_int(FNV_OFFSET, false))
            .llvm_ctx("initialize byte hash")?;
        let check = self.block("key.bytes.check");
        let body = self.block("key.bytes.byte");
        let done = self.block("key.bytes.done");
        self.jump(check)?;
        self.builder.position_at_end(check);
        let i = self.load(size.into(), index)?.into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, i, length, "key.bytes.more")
            .llvm_ctx("check byte cursor")?;
        self.branch(more, body, done)?;
        self.builder.position_at_end(body);
        let address_index = self
            .builder
            .build_int_add(offset, i, "key.bytes.address.index")
            .llvm_ctx("address active byte")?;
        // SAFETY: the loop bounds stay inside the checked live Bytes region.
        let address = unsafe {
            self.builder.build_in_bounds_gep(
                ctx.i8_type(),
                data,
                &[address_index],
                "key.bytes.address",
            )
        }
        .llvm_ctx("address byte")?;
        let byte = self.load(ctx.i8_type().into(), address)?.into_int_value();
        let byte = self
            .builder
            .build_int_z_extend(byte, ctx.i64_type(), "key.byte.bits")
            .llvm_ctx("widen byte")?;
        let mixed = self.mix(
            self.load(ctx.i64_type().into(), hash)?.into_int_value(),
            byte,
        )?;
        self.builder
            .build_store(hash, mixed)
            .llvm_ctx("accumulate byte hash")?;
        let next = self
            .builder
            .build_int_add(i, size.const_int(1, false), "key.bytes.next")
            .llvm_ctx("advance byte cursor")?;
        self.builder
            .build_store(index, next)
            .llvm_ctx("store byte cursor")?;
        self.jump(check)?;
        self.builder.position_at_end(done);
        self.finish(self.load(ctx.i64_type().into(), hash)?.into_int_value())
    }

    fn mix(
        &self,
        state: IntValue<'ctx>,
        component: IntValue<'ctx>,
    ) -> CodegenResult<IntValue<'ctx>> {
        let bits = self
            .builder
            .build_xor(state, component, "key.hash.xor")
            .llvm_ctx("combine selected component hash")?;
        self.builder
            .build_int_mul(
                bits,
                self.parent.ctx.i64_type().const_int(FNV_PRIME, false),
                "key.hash.mix",
            )
            .llvm_ctx("mix selected component hash")
    }

    fn fields(
        &self,
        repr: &PhysicalRepr,
        fields: &[PhysicalValueRecipe],
        lhs: PointerValue<'ctx>,
        rhs: Option<PointerValue<'ctx>>,
    ) -> CodegenResult<()> {
        let structure = llvm_type(self.parent.ctx, repr)?.into_struct_type();
        let mut state = self.parent.ctx.i64_type().const_int(FNV_OFFSET, false);
        for (index, field) in fields.iter().enumerate() {
            let index = u32::try_from(index).map_err(|_| {
                CodegenError::FailClosed("key field index exceeds LLVM range".into())
            })?;
            let left = self
                .builder
                .build_struct_gep(structure, lhs, index, "key.field.left")
                .llvm_ctx("address semantic key field")?;
            let right = rhs
                .map(|rhs| {
                    self.builder
                        .build_struct_gep(structure, rhs, index, "key.field.right")
                        .llvm_ctx("address semantic key field")
                })
                .transpose()?;
            let value = self.component(&field.ty, left, right)?;
            if self.capability == ValueCapability::Hash {
                state = self.mix(state, value)?;
            } else {
                self.continue_equal(value)?;
            }
        }
        self.finish(if rhs.is_some() {
            self.parent.ctx.bool_type().const_int(1, false)
        } else {
            state
        })
    }

    fn variant(
        &self,
        id: PhysicalVariantId,
        lhs: PointerValue<'ctx>,
        rhs: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let values = self.values();
        let glue = values.variant_glue(id)?;
        let layout = values.variant_layout(&glue.ty)?;
        if layout.is_indirect {
            return Err(CodegenError::FailClosed(
                "indirect variant key equality is outside checker admission".into(),
            ));
        }
        let object_ty = llvm_type(self.parent.ctx, &layout.object.repr)?.into_struct_type();
        let left_tag = self
            .builder
            .build_struct_gep(object_ty, lhs, 0, "key.variant.left.tag")
            .llvm_ctx("address variant tag")?;
        let right_tag = self
            .builder
            .build_struct_gep(object_ty, rhs, 0, "key.variant.right.tag")
            .llvm_ctx("address variant tag")?;
        let tag_ty = object_ty
            .get_field_type_at_index(0)
            .ok_or_else(|| CodegenError::FailClosed("variant key has no tag".into()))?;
        let tag = self.load(tag_ty, left_tag)?.into_int_value();
        self.continue_equal(self.equal(tag, self.load(tag_ty, right_tag)?.into_int_value())?)?;
        let invalid = self.block("key.variant.invalid");
        let cases = glue
            .variants
            .iter()
            .enumerate()
            .map(|(index, _)| {
                (
                    tag.get_type().const_int(index as u64, false),
                    self.block("key.variant.case"),
                )
            })
            .collect::<Vec<_>>();
        self.builder
            .build_switch(tag, invalid, &cases)
            .llvm_ctx("dispatch selected variant equality")?;
        for (index, (_, block)) in cases.iter().enumerate() {
            self.builder.position_at_end(*block);
            self.fields(
                &layout.variants[index].repr,
                &glue.variants[index].fields,
                values.variant_payload_ptr(lhs, layout)?,
                Some(values.variant_payload_ptr(rhs, layout)?),
            )?;
        }
        self.builder.position_at_end(invalid);
        values.emit_invalid_variant_tag()
    }

    fn length(&self, symbol: &str, handle: PointerValue<'ctx>) -> CodegenResult<IntValue<'ctx>> {
        let ptr = self.parent.ctx.ptr_type(AddressSpace::default());
        Ok(self
            .runtime(
                symbol,
                self.parent.ctx.i64_type().fn_type(&[ptr.into()], false),
                &[handle.into()],
            )?
            .into_int_value())
    }

    fn vector(
        &self,
        element: &ResolvedTy,
        lhs: PointerValue<'ctx>,
        rhs: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let ctx = self.parent.ctx;
        let ptr = ctx.ptr_type(AddressSpace::default());
        let left = self.handle(lhs)?;
        let right = self.handle(rhs)?;
        let len = self.length("hew_vec_len", left)?;
        self.continue_equal(self.equal(len, self.length("hew_vec_len", right)?)?)?;
        let index = self
            .values()
            .entry_scratch(ctx.i64_type().into(), "key.vector.index")?;
        self.builder
            .build_store(index, ctx.i64_type().const_zero())
            .llvm_ctx("initialize vector cursor")?;
        let check = self.block("key.vector.check");
        let body = self.block("key.vector.element");
        let done = self.block("key.vector.done");
        self.jump(check)?;
        self.builder.position_at_end(check);
        let i = self.load(ctx.i64_type().into(), index)?.into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, i, len, "key.vector.more")
            .llvm_ctx("check vector cursor")?;
        self.branch(more, body, done)?;
        self.builder.position_at_end(body);
        let get_ty = ptr.fn_type(&[ptr.into(), ctx.i64_type().into()], false);
        let left_slot = self
            .runtime("hew_vec_get_owned", get_ty, &[left.into(), i.into()])?
            .into_pointer_value();
        let right_slot = self
            .runtime("hew_vec_get_owned", get_ty, &[right.into(), i.into()])?
            .into_pointer_value();
        self.continue_equal(self.component(element, left_slot, Some(right_slot))?)?;
        let next = self
            .builder
            .build_int_add(i, ctx.i64_type().const_int(1, false), "key.vector.next")
            .llvm_ctx("advance vector cursor")?;
        self.builder
            .build_store(index, next)
            .llvm_ctx("store vector cursor")?;
        self.jump(check)?;
        self.builder.position_at_end(done);
        self.finish(ctx.bool_type().const_int(1, false))
    }
}

#[cfg(test)]
#[path = "physical_key_tests.rs"]
mod tests;
