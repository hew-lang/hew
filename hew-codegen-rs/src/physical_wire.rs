//! Native callbacks realize a single semantic wire schema with physical value glue.

use super::*;
use hew_mir::physical::{SemWireKind, SemWirePlan};
use hew_types::{WireCodecDirection, WireFieldPresence, WireTextFormat};

fn wire_symbol(plan: &SemWirePlan, decode: bool) -> String {
    format!(
        "__hew_wire_{}_{}",
        if decode { "decode" } else { "encode" },
        hew_types::mangle_resolved_ty(&plan.ty)
    )
}

fn text_descriptor(plan: &SemWirePlan, yaml: bool) -> CodegenResult<serde_json::Value> {
    use serde_json::json;
    Ok(match &plan.kind {
        SemWireKind::Scalar => json!({"k": match plan.ty {
            ResolvedTy::I8 | ResolvedTy::I16 | ResolvedTy::I32 | ResolvedTy::I64 | ResolvedTy::Isize | ResolvedTy::Duration | ResolvedTy::Char => "i64",
            ResolvedTy::U8 | ResolvedTy::U16 | ResolvedTy::U32 | ResolvedTy::U64 | ResolvedTy::Usize => "u64",
            ResolvedTy::F32 | ResolvedTy::F64 => "f64", ResolvedTy::Bool => "bool",
            ResolvedTy::String => "str", ResolvedTy::Bytes => "bytes",
            _ => return Err(CodegenError::FailClosed("wire scalar has no text schema".into())),
        }}),
        SemWireKind::Vector(value) => json!({"k":"vec", "e":text_descriptor(value, yaml)?}),
        SemWireKind::Set(value) => json!({"k":"set", "e":text_descriptor(value, yaml)?}),
        SemWireKind::Option { value, .. } => json!({"k":"opt", "e":text_descriptor(value, yaml)?}),
        SemWireKind::Map { key, value } => {
            json!({"k":"map", "key":text_descriptor(key, yaml)?, "value":text_descriptor(value, yaml)?})
        }
        SemWireKind::Record { fields, .. } => {
            json!({"k":"struct", "f": fields.iter().map(|field| Ok(json!({
            "t":field.tag, "n":if yaml { &field.yaml_name } else { &field.json_name },
            "p":if field.presence == WireFieldPresence::Required { "required" } else { "optional" },
            "d":text_descriptor(&field.value, yaml)?,
        }))).collect::<CodegenResult<Vec<_>>>()?})
        }
        SemWireKind::Enum { variants, .. } => {
            json!({"k":"enum", "v": variants.iter().map(|variant| Ok(json!({
            "t":variant.tag, "n":if yaml { &variant.yaml_name } else { &variant.json_name },
            "p":variant.fields.iter().map(|field| text_descriptor(field, yaml)).collect::<CodegenResult<Vec<_>>>()?,
        }))).collect::<CodegenResult<Vec<_>>>()?})
        }
    })
}

fn runtime<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    name: &str,
    result: Option<BasicTypeEnum<'ctx>>,
    args: &[BasicValueEnum<'ctx>],
) -> CodegenResult<Option<BasicValueEnum<'ctx>>> {
    let params = args
        .iter()
        .map(|arg| arg.get_type().into())
        .collect::<Vec<_>>();
    let signature = result.map_or_else(
        || values.ctx.void_type().fn_type(&params, false),
        |ty| ty.fn_type(&params, false),
    );
    let function = get_or_declare_external(values.llvm, name, signature)?;
    let args = args.iter().copied().map(Into::into).collect::<Vec<_>>();
    let call = values
        .builder
        .build_call(
            function,
            &args,
            if result.is_some() { "wire.runtime" } else { "" },
        )
        .llvm_ctx("call wire runtime")?;
    Ok(call.try_as_basic_value().basic())
}

fn runtime_value<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    name: &str,
    result: BasicTypeEnum<'ctx>,
    args: &[BasicValueEnum<'ctx>],
) -> CodegenResult<BasicValueEnum<'ctx>> {
    runtime(values, name, Some(result), args)?
        .ok_or_else(|| CodegenError::FailClosed("wire runtime returned no value".into()))
}

fn constant_text<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    bytes: &[u8],
    nul: bool,
) -> PointerValue<'ctx> {
    let constant = values.ctx.const_string(bytes, nul);
    let global = values
        .llvm
        .add_global(constant.get_type(), None, "wire.text");
    global.set_linkage(Linkage::Private);
    global.set_constant(true);
    global.set_initializer(&constant);
    global.as_pointer_value()
}

fn emit_callback<'ctx>(
    module: &PhysicalModule,
    ctx: &'ctx Context,
    llvm: &Module<'ctx>,
    plan: &SemWirePlan,
    recipes: &BTreeMap<ResolvedTy, PhysicalValueRecipe>,
    decode: bool,
) -> CodegenResult<FunctionValue<'ctx>> {
    let name = wire_symbol(plan, decode);
    if let Some(function) = llvm.get_function(&name) {
        return Ok(function);
    }
    let pointer = ctx.ptr_type(AddressSpace::default());
    let signature = if decode {
        ctx.i32_type().fn_type(&[pointer.into(); 3], false)
    } else {
        ctx.void_type().fn_type(&[pointer.into(); 2], false)
    };
    let function = llvm.add_function(&name, signature, Some(Linkage::Internal));
    let entry = ctx.append_basic_block(function, "entry");
    let body = ctx.append_basic_block(function, "body");
    let builder = ctx.create_builder();
    builder.position_at_end(entry);
    builder
        .build_unconditional_branch(body)
        .llvm_ctx("enter wire callback")?;
    builder.position_at_end(body);
    let values = ValueEmitter {
        module,
        ctx,
        llvm,
        builder: &builder,
        value: function,
    };
    let cursor = function
        .get_nth_param(0)
        .expect("wire callback cursor")
        .into_pointer_value();
    let slot = function
        .get_nth_param(1)
        .expect("wire callback value")
        .into_pointer_value();
    if decode {
        let fault = function
            .get_nth_param(2)
            .expect("wire callback fault output")
            .into_pointer_value();
        let fail = ctx.append_basic_block(function, "rollback");
        let status = values.entry_scratch(ctx.i32_type().into(), "wire.status")?;
        builder
            .build_store(status, ctx.i32_type().const_int(1, false))
            .llvm_ctx("initialize decode failure status")?;
        let mut emitter = DecodeEmitter {
            values,
            recipes,
            cursor,
            fault,
            fail,
            status,
            owners: Vec::new(),
        };
        let temporary = emitter.temporary(&plan.ty)?;
        emitter.decode(plan, temporary)?;
        emitter.check_cursor()?;
        let loaded = emitter.load(temporary.slot, &plan.ty)?;
        builder
            .build_store(slot, loaded)
            .llvm_ctx("publish complete decoded value")?;
        emitter.mark(temporary, false)?;
        builder
            .build_return(Some(&ctx.i32_type().const_zero()))
            .llvm_ctx("finish decoded value")?;
        emitter.rollback()?;
    } else {
        EncodeEmitter {
            values,
            recipes,
            cursor,
        }
        .encode(plan, slot)?;
        builder
            .build_return(None)
            .llvm_ctx("finish wire encode callback")?;
    }
    Ok(function)
}

struct EncodeEmitter<'a, 'ctx> {
    values: ValueEmitter<'a, 'ctx>,
    recipes: &'a BTreeMap<ResolvedTy, PhysicalValueRecipe>,
    cursor: PointerValue<'ctx>,
}

impl<'ctx> EncodeEmitter<'_, 'ctx> {
    fn void(&self, name: &str, args: &[BasicValueEnum<'ctx>]) -> CodegenResult<()> {
        let args = std::iter::once(self.cursor.into())
            .chain(args.iter().copied())
            .collect::<Vec<_>>();
        runtime(&self.values, name, None, &args).map(|_| ())
    }
    fn child(&self, plan: &SemWirePlan, slot: PointerValue<'ctx>) -> CodegenResult<()> {
        let function = emit_callback(
            self.values.module,
            self.values.ctx,
            self.values.llvm,
            plan,
            self.recipes,
            false,
        )?;
        self.values
            .builder
            .build_call(function, &[self.cursor.into(), slot.into()], "")
            .llvm_ctx("encode checked wire child")?;
        Ok(())
    }
    fn load(
        &self,
        slot: PointerValue<'ctx>,
        ty: &ResolvedTy,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let layout =
            self.values.module.target.layout(ty).ok_or_else(|| {
                CodegenError::FailClosed("wire type has no physical layout".into())
            })?;
        self.values
            .builder
            .build_load(
                llvm_type(self.values.ctx, &layout.repr)?,
                slot,
                "wire.value",
            )
            .llvm_ctx("read borrowed wire value")
    }
    fn tag(
        &self,
        slot: PointerValue<'ctx>,
        ty: &ResolvedTy,
    ) -> CodegenResult<(IntValue<'ctx>, PointerValue<'ctx>)> {
        let layout = self.values.variant_layout(ty)?;
        let object = self.values.variant_object_ptr(slot, layout)?;
        Ok((self.values.load_variant_tag(object, layout)?, object))
    }
    fn payload(
        &self,
        object: PointerValue<'ctx>,
        ty: &ResolvedTy,
        variant: u32,
        field: u32,
    ) -> CodegenResult<PointerValue<'ctx>> {
        variant_field_pointer(&self.values, object, ty, variant, field)
    }
    fn encode(&self, plan: &SemWirePlan, slot: PointerValue<'ctx>) -> CodegenResult<()> {
        let ctx = self.values.ctx;
        let builder = self.values.builder;
        match &plan.kind {
            SemWireKind::Scalar => {
                let value = self.load(slot, &plan.ty)?;
                let (name, value) = match plan.ty {
                    ResolvedTy::Bool => ("hew_cbor_ser_bool", value),
                    ResolvedTy::String => ("hew_cbor_ser_string_hew", value),
                    ResolvedTy::Bytes => {
                        let triple = value.into_struct_value();
                        let args = (0..3)
                            .map(|index| {
                                builder
                                    .build_extract_value(triple, index, "wire.bytes.field")
                                    .llvm_ctx("read byte carrier")
                            })
                            .collect::<CodegenResult<Vec<_>>>()?;
                        return self.void("hew_cbor_ser_bytes", &args);
                    }
                    ResolvedTy::F32 => (
                        "hew_cbor_ser_f64",
                        builder
                            .build_float_ext(value.into_float_value(), ctx.f64_type(), "wire.float")
                            .llvm_ctx("widen wire float")?
                            .into(),
                    ),
                    ResolvedTy::F64 => ("hew_cbor_ser_f64", value),
                    _ => {
                        let signed = matches!(
                            plan.ty,
                            ResolvedTy::I8
                                | ResolvedTy::I16
                                | ResolvedTy::I32
                                | ResolvedTy::I64
                                | ResolvedTy::Isize
                                | ResolvedTy::Duration
                        );
                        let value = value.into_int_value();
                        let wide = builder
                            .build_int_cast_sign_flag(value, ctx.i64_type(), signed, "wire.integer")
                            .llvm_ctx("widen wire integer")?;
                        (
                            if signed {
                                "hew_cbor_ser_i64"
                            } else {
                                "hew_cbor_ser_u64"
                            },
                            wide.into(),
                        )
                    }
                };
                self.void(name, &[value])
            }
            SemWireKind::Record { fields, .. } => {
                self.void("hew_cbor_ser_begin_map", &[])?;
                let layout =
                    self.values.module.target.layout(&plan.ty).ok_or_else(|| {
                        CodegenError::FailClosed("wire record has no layout".into())
                    })?;
                let ty = llvm_type(ctx, &layout.repr)?.into_struct_type();
                for field in fields {
                    let field_slot = builder
                        .build_struct_gep(ty, slot, field.index, "wire.field")
                        .llvm_ctx("address wire record field")?;
                    let continuation = if field.presence == WireFieldPresence::Optional {
                        let SemWireKind::Option { none, .. } = field.value.kind else {
                            return Err(CodegenError::FailClosed(
                                "optional wire key lacks Option value".into(),
                            ));
                        };
                        let (tag, _) = self.tag(field_slot, &field.value.ty)?;
                        let absent = builder
                            .build_int_compare(
                                IntPredicate::EQ,
                                tag,
                                tag.get_type().const_int(u64::from(none), false),
                                "wire.absent",
                            )
                            .llvm_ctx("test optional key presence")?;
                        let write = ctx.append_basic_block(self.values.value, "wire.key.present");
                        let next = ctx.append_basic_block(self.values.value, "wire.key.next");
                        builder
                            .build_conditional_branch(absent, next, write)
                            .llvm_ctx("omit absent optional key")?;
                        builder.position_at_end(write);
                        Some(next)
                    } else {
                        None
                    };
                    self.void(
                        "hew_cbor_ser_key_u64",
                        &[ctx.i64_type().const_int(u64::from(field.tag), false).into()],
                    )?;
                    self.child(&field.value, field_slot)?;
                    if let Some(next) = continuation {
                        builder
                            .build_unconditional_branch(next)
                            .llvm_ctx("finish optional key")?;
                        builder.position_at_end(next);
                    }
                }
                self.void("hew_cbor_ser_end_map", &[])
            }
            SemWireKind::Option {
                none, some, value, ..
            } => {
                let (tag, object) = self.tag(slot, &plan.ty)?;
                let empty = ctx.append_basic_block(self.values.value, "wire.none");
                let present = ctx.append_basic_block(self.values.value, "wire.some");
                let invalid = ctx.append_basic_block(self.values.value, "wire.invalid.option");
                let done = ctx.append_basic_block(self.values.value, "wire.option.done");
                builder
                    .build_switch(
                        tag,
                        invalid,
                        &[
                            (tag.get_type().const_int(u64::from(*none), false), empty),
                            (tag.get_type().const_int(u64::from(*some), false), present),
                        ],
                    )
                    .llvm_ctx("select checked Option variant")?;
                builder.position_at_end(invalid);
                self.values.emit_invalid_variant_tag()?;
                builder.position_at_end(empty);
                self.void("hew_cbor_ser_null", &[])?;
                builder
                    .build_unconditional_branch(done)
                    .llvm_ctx("finish None encoding")?;
                builder.position_at_end(present);
                self.child(value, self.payload(object, &plan.ty, *some, 0)?)?;
                builder
                    .build_unconditional_branch(done)
                    .llvm_ctx("finish Some encoding")?;
                builder.position_at_end(done);
                Ok(())
            }
            SemWireKind::Enum { variants, .. } => {
                let (tag, object) = self.tag(slot, &plan.ty)?;
                let invalid = ctx.append_basic_block(self.values.value, "wire.invalid.enum");
                let done = ctx.append_basic_block(self.values.value, "wire.enum.done");
                let cases = variants
                    .iter()
                    .map(|variant| {
                        (
                            tag.get_type().const_int(u64::from(variant.index), false),
                            ctx.append_basic_block(self.values.value, "wire.variant"),
                        )
                    })
                    .collect::<Vec<_>>();
                builder
                    .build_switch(tag, invalid, &cases)
                    .llvm_ctx("select wire enum variant")?;
                builder.position_at_end(invalid);
                self.values.emit_invalid_variant_tag()?;
                for (variant, (_, block)) in variants.iter().zip(cases) {
                    builder.position_at_end(block);
                    let wire_tag = ctx
                        .i64_type()
                        .const_int(u64::from(variant.tag), false)
                        .into();
                    if variant.fields.is_empty() {
                        self.void("hew_cbor_ser_u64", &[wire_tag])?;
                    } else {
                        self.void("hew_cbor_ser_begin_map", &[])?;
                        self.void("hew_cbor_ser_key_u64", &[wire_tag])?;
                        self.void("hew_cbor_ser_begin_array", &[])?;
                        for (index, field) in variant.fields.iter().enumerate() {
                            self.child(
                                field,
                                self.payload(
                                    object,
                                    &plan.ty,
                                    variant.index,
                                    u32::try_from(index).map_err(|_| {
                                        CodegenError::FailClosed(
                                            "wire payload index exceeds u32".into(),
                                        )
                                    })?,
                                )?,
                            )?;
                        }
                        self.void("hew_cbor_ser_end_array", &[])?;
                        self.void("hew_cbor_ser_end_map", &[])?;
                    }
                    builder
                        .build_unconditional_branch(done)
                        .llvm_ctx("finish enum encoding")?;
                }
                builder.position_at_end(done);
                Ok(())
            }
            SemWireKind::Vector(value) => self.vector(plan, value, slot),
            SemWireKind::Set(value) => self.associative(plan, value, None, slot),
            SemWireKind::Map { key, value } => self.associative(plan, key, Some(value), slot),
        }
    }
    fn vector(
        &self,
        plan: &SemWirePlan,
        element: &SemWirePlan,
        slot: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let ctx = self.values.ctx;
        let builder = self.values.builder;
        let vector = self.load(slot, &plan.ty)?;
        let len = runtime_value(
            &self.values,
            "hew_vec_len",
            ctx.i64_type().into(),
            &[vector],
        )?
        .into_int_value();
        let index = self
            .values
            .entry_scratch(ctx.i64_type().into(), "wire.index")?;
        builder
            .build_store(index, ctx.i64_type().const_zero())
            .llvm_ctx("initialize wire index")?;
        self.void("hew_cbor_ser_begin_array", &[])?;
        let header = ctx.append_basic_block(self.values.value, "wire.vector.next");
        let body = ctx.append_basic_block(self.values.value, "wire.vector.element");
        let done = ctx.append_basic_block(self.values.value, "wire.vector.done");
        builder
            .build_unconditional_branch(header)
            .llvm_ctx("enter vector encode loop")?;
        builder.position_at_end(header);
        let current = builder
            .build_load(ctx.i64_type(), index, "wire.index")
            .llvm_ctx("read vector index")?
            .into_int_value();
        let present = builder
            .build_int_compare(IntPredicate::ULT, current, len, "wire.vector.present")
            .llvm_ctx("check vector encode bound")?;
        builder
            .build_conditional_branch(present, body, done)
            .llvm_ctx("iterate vector encoding")?;
        builder.position_at_end(body);
        let ptr = runtime_value(
            &self.values,
            "hew_vec_get_owned",
            ctx.ptr_type(AddressSpace::default()).into(),
            &[vector, current.into()],
        )?
        .into_pointer_value();
        self.child(element, ptr)?;
        let next = builder
            .build_int_add(
                current,
                ctx.i64_type().const_int(1, false),
                "wire.index.next",
            )
            .llvm_ctx("advance wire vector index")?;
        builder
            .build_store(index, next)
            .llvm_ctx("store wire vector index")?;
        builder
            .build_unconditional_branch(header)
            .llvm_ctx("continue vector encoding")?;
        builder.position_at_end(done);
        self.void("hew_cbor_ser_end_array", &[])
    }
    fn associative(
        &self,
        plan: &SemWirePlan,
        key: &SemWirePlan,
        value: Option<&SemWirePlan>,
        slot: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let ctx = self.values.ctx;
        let builder = self.values.builder;
        let pointer = ctx.ptr_type(AddressSpace::default());
        let map = value.is_some();
        let prefix = if map { "hew_hashmap" } else { "hew_hashset" };
        let collection = self.load(slot, &plan.ty)?;
        let cursor = runtime_value(
            &self.values,
            &format!("{prefix}_iter_new_layout"),
            pointer.into(),
            &[collection],
        )?;
        let key_out = self.values.entry_scratch(pointer.into(), "wire.key.ptr")?;
        let value_out = self
            .values
            .entry_scratch(pointer.into(), "wire.value.ptr")?;
        self.void(
            if map {
                "hew_cbor_ser_begin_map"
            } else {
                "hew_cbor_ser_begin_set"
            },
            &[],
        )?;
        let header = ctx.append_basic_block(self.values.value, "wire.collection.next");
        let body = ctx.append_basic_block(self.values.value, "wire.collection.element");
        let done = ctx.append_basic_block(self.values.value, "wire.collection.done");
        builder
            .build_unconditional_branch(header)
            .llvm_ctx("enter wire collection loop")?;
        builder.position_at_end(header);
        let mut args = vec![cursor, key_out.into()];
        if map {
            args.push(value_out.into());
        }
        let present = runtime_value(
            &self.values,
            &format!("{prefix}_iter_next_layout"),
            ctx.bool_type().into(),
            &args,
        )?
        .into_int_value();
        builder
            .build_conditional_branch(present, body, done)
            .llvm_ctx("advance wire collection cursor")?;
        builder.position_at_end(body);
        if map {
            self.void("hew_cbor_ser_begin_key", &[])?;
        }
        let key_slot = builder
            .build_load(pointer, key_out, "wire.key")
            .llvm_ctx("read borrowed wire key")?
            .into_pointer_value();
        self.child(key, key_slot)?;
        if let Some(value) = value {
            let value_slot = builder
                .build_load(pointer, value_out, "wire.value")
                .llvm_ctx("read borrowed wire map value")?
                .into_pointer_value();
            self.child(value, value_slot)?;
        }
        builder
            .build_unconditional_branch(header)
            .llvm_ctx("continue collection encoding")?;
        builder.position_at_end(done);
        runtime(
            &self.values,
            &format!("{prefix}_iter_free_layout"),
            None,
            &[cursor],
        )?;
        self.void(
            if map {
                "hew_cbor_ser_end_map"
            } else {
                "hew_cbor_ser_end_array"
            },
            &[],
        )
    }
}

fn variant_field_pointer<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    object: PointerValue<'ctx>,
    ty: &ResolvedTy,
    variant: u32,
    field: u32,
) -> CodegenResult<PointerValue<'ctx>> {
    let layout = values.variant_layout(ty)?;
    let payload = values.variant_payload_ptr(object, layout)?;
    let layout = layout
        .variants
        .get(variant as usize)
        .ok_or_else(|| CodegenError::FailClosed("wire variant has no physical payload".into()))?;
    values
        .builder
        .build_struct_gep(
            llvm_type(values.ctx, &layout.repr)?.into_struct_type(),
            payload,
            field,
            "wire.payload.field",
        )
        .llvm_ctx("address checked wire payload field")
}

#[derive(Clone, Copy)]
struct DecodeTemporary<'ctx> {
    slot: PointerValue<'ctx>,
    owner: Option<usize>,
}

struct DecodeOwner<'ctx> {
    slot: PointerValue<'ctx>,
    initialized: PointerValue<'ctx>,
    recipe: PhysicalValueRecipe,
}

struct DecodeEmitter<'a, 'ctx> {
    values: ValueEmitter<'a, 'ctx>,
    recipes: &'a BTreeMap<ResolvedTy, PhysicalValueRecipe>,
    cursor: PointerValue<'ctx>,
    fault: PointerValue<'ctx>,
    fail: BasicBlock<'ctx>,
    status: PointerValue<'ctx>,
    owners: Vec<DecodeOwner<'ctx>>,
}

impl<'ctx> DecodeEmitter<'_, 'ctx> {
    fn temporary(&mut self, ty: &ResolvedTy) -> CodegenResult<DecodeTemporary<'ctx>> {
        let recipe = self.recipes.get(ty).ok_or_else(|| {
            CodegenError::FailClosed("wire decode lacks exact value recipe".into())
        })?;
        let layout = self.values.module.target.layout(ty).ok_or_else(|| {
            CodegenError::FailClosed("wire decode lacks physical type layout".into())
        })?;
        let slot = self
            .values
            .entry_scratch(llvm_type(self.values.ctx, &layout.repr)?, "wire.temporary")?;
        let owner = if recipe.destroy.is_some() {
            let initialized = self
                .values
                .entry_scratch(self.values.ctx.bool_type().into(), "wire.initialized")?;
            let prologue = self
                .values
                .value
                .get_first_basic_block()
                .expect("wire callback has prologue");
            let builder = self.values.ctx.create_builder();
            builder.position_before(
                &prologue
                    .get_terminator()
                    .expect("wire prologue branches to body"),
            );
            builder
                .build_store(initialized, self.values.ctx.bool_type().const_zero())
                .llvm_ctx("initialize wire cleanup obligation")?;
            let owner = self.owners.len();
            self.owners.push(DecodeOwner {
                slot,
                initialized,
                recipe: recipe.clone(),
            });
            Some(owner)
        } else {
            None
        };
        Ok(DecodeTemporary { slot, owner })
    }
    fn mark(&self, temporary: DecodeTemporary<'ctx>, initialized: bool) -> CodegenResult<()> {
        if let Some(owner) = temporary.owner {
            self.values
                .builder
                .build_store(
                    self.owners[owner].initialized,
                    self.values
                        .ctx
                        .bool_type()
                        .const_int(u64::from(initialized), false),
                )
                .llvm_ctx("transfer wire cleanup obligation")?;
        }
        Ok(())
    }
    fn release(&self, temporary: DecodeTemporary<'ctx>) -> CodegenResult<()> {
        if let Some(owner) = temporary.owner {
            self.destroy(&self.owners[owner])?;
        }
        self.mark(temporary, false)
    }
    fn destroy(&self, owner: &DecodeOwner<'ctx>) -> CodegenResult<()> {
        let layout = self
            .values
            .module
            .target
            .layout(&owner.recipe.ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("wire cleanup has no physical layout".into())
            })?;
        let value = self
            .values
            .builder
            .build_load(
                llvm_type(self.values.ctx, &layout.repr)?,
                owner.slot,
                "wire.partial.owner",
            )
            .llvm_ctx("read initialized wire owner")?;
        self.values.destroy_loaded_value(
            value,
            layout,
            owner
                .recipe
                .destroy
                .expect("owned temporary has destruction"),
        )
    }
    fn rollback(&self) -> CodegenResult<()> {
        self.values.builder.position_at_end(self.fail);
        for owner in self.owners.iter().rev() {
            let release = self
                .values
                .ctx
                .append_basic_block(self.values.value, "wire.release");
            let next = self
                .values
                .ctx
                .append_basic_block(self.values.value, "wire.release.next");
            let initialized = self
                .values
                .builder
                .build_load(self.values.ctx.bool_type(), owner.initialized, "wire.live")
                .llvm_ctx("test partial wire owner")?
                .into_int_value();
            self.values
                .builder
                .build_conditional_branch(initialized, release, next)
                .llvm_ctx("release only initialized wire fields")?;
            self.values.builder.position_at_end(release);
            self.destroy(owner)?;
            self.values
                .builder
                .build_unconditional_branch(next)
                .llvm_ctx("continue wire rollback")?;
            self.values.builder.position_at_end(next);
        }
        let status = self
            .values
            .builder
            .build_load(
                self.values.ctx.i32_type(),
                self.status,
                "wire.failure.status",
            )
            .llvm_ctx("read wire failure status")?;
        self.values
            .builder
            .build_return(Some(&status))
            .llvm_ctx("return failed wire decode")?;
        Ok(())
    }
    fn load(
        &self,
        slot: PointerValue<'ctx>,
        ty: &ResolvedTy,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let layout = self
            .values
            .module
            .target
            .layout(ty)
            .ok_or_else(|| CodegenError::FailClosed("wire value has no layout".into()))?;
        self.values
            .builder
            .build_load(
                llvm_type(self.values.ctx, &layout.repr)?,
                slot,
                "wire.decoded",
            )
            .llvm_ctx("read complete wire value")
    }
    fn void(&self, name: &str, args: &[BasicValueEnum<'ctx>]) -> CodegenResult<()> {
        let args = std::iter::once(self.cursor.into())
            .chain(args.iter().copied())
            .collect::<Vec<_>>();
        runtime(&self.values, name, None, &args).map(|_| ())
    }
    fn read(
        &self,
        name: &str,
        ty: BasicTypeEnum<'ctx>,
        args: &[BasicValueEnum<'ctx>],
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let args = std::iter::once(self.cursor.into())
            .chain(args.iter().copied())
            .collect::<Vec<_>>();
        runtime_value(&self.values, name, ty, &args)
    }
    fn check_status(&self, status: IntValue<'ctx>) -> CodegenResult<()> {
        let success = self
            .values
            .ctx
            .append_basic_block(self.values.value, "wire.checked");
        self.values
            .builder
            .build_store(self.status, status)
            .llvm_ctx("retain wire failure status")?;
        let failed = self
            .values
            .builder
            .build_int_compare(
                IntPredicate::NE,
                status,
                status.get_type().const_zero(),
                "wire.failed",
            )
            .llvm_ctx("test wire callback status")?;
        self.values
            .builder
            .build_conditional_branch(failed, self.fail, success)
            .llvm_ctx("reject incomplete wire value")?;
        self.values.builder.position_at_end(success);
        Ok(())
    }
    fn check_cursor(&self) -> CodegenResult<()> {
        let status = self
            .read("hew_cbor_de_failed", self.values.ctx.i32_type().into(), &[])?
            .into_int_value();
        self.check_status(status)
    }
    fn fail_now(&self) -> CodegenResult<()> {
        self.values
            .builder
            .build_store(self.status, self.values.ctx.i32_type().const_int(1, false))
            .llvm_ctx("record malformed wire value")?;
        self.values
            .builder
            .build_unconditional_branch(self.fail)
            .llvm_ctx("reject malformed wire value")?;
        Ok(())
    }
    fn child_into(
        &self,
        plan: &SemWirePlan,
        temporary: DecodeTemporary<'ctx>,
    ) -> CodegenResult<()> {
        let function = emit_callback(
            self.values.module,
            self.values.ctx,
            self.values.llvm,
            plan,
            self.recipes,
            true,
        )?;
        let status = self
            .values
            .builder
            .build_call(
                function,
                &[self.cursor.into(), temporary.slot.into(), self.fault.into()],
                "wire.child.status",
            )
            .llvm_ctx("decode exact wire child")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("wire decoder returned no status".into()))?
            .into_int_value();
        self.check_status(status)?;
        self.mark(temporary, true)
    }
    fn child(&mut self, plan: &SemWirePlan) -> CodegenResult<DecodeTemporary<'ctx>> {
        let temporary = self.temporary(&plan.ty)?;
        self.child_into(plan, temporary)?;
        Ok(temporary)
    }
    fn variant(
        &self,
        plan: &SemWirePlan,
        variant: u32,
        fields: &[(DecodeTemporary<'ctx>, &SemWirePlan)],
        output: DecodeTemporary<'ctx>,
    ) -> CodegenResult<()> {
        let glue = self
            .values
            .module
            .variant_glue
            .iter()
            .find(|glue| glue.ty == plan.ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("wire variant has no exact physical glue".into())
            })?;
        let values = fields
            .iter()
            .map(|(value, plan)| self.load(value.slot, &plan.ty))
            .collect::<CodegenResult<Vec<_>>>()?;
        self.values
            .write_variant_value(output.slot, variant, &values, glue.id)?;
        self.mark(output, true)?;
        for (field, _) in fields {
            self.mark(*field, false)?;
        }
        Ok(())
    }
    fn decode(&mut self, plan: &SemWirePlan, output: DecodeTemporary<'ctx>) -> CodegenResult<()> {
        let ctx = self.values.ctx;
        let builder = self.values.builder;
        match &plan.kind {
            SemWireKind::Scalar => {
                let layout =
                    self.values.module.target.layout(&plan.ty).ok_or_else(|| {
                        CodegenError::FailClosed("wire scalar has no layout".into())
                    })?;
                let ty = llvm_type(ctx, &layout.repr)?;
                let value = match plan.ty {
                    ResolvedTy::Bool => self.read("hew_cbor_de_bool", ctx.i8_type().into(), &[])?,
                    ResolvedTy::String => self.read(
                        "hew_cbor_de_string_hew",
                        ctx.ptr_type(AddressSpace::default()).into(),
                        &[],
                    )?,
                    ResolvedTy::Bytes => {
                        self.void("hew_cbor_de_bytes_hew", &[output.slot.into()])?;
                        return self.mark(output, true);
                    }
                    ResolvedTy::F32 | ResolvedTy::F64 => {
                        let value = self
                            .read("hew_cbor_de_f64", ctx.f64_type().into(), &[])?
                            .into_float_value();
                        if plan.ty == ResolvedTy::F32 {
                            builder
                                .build_float_trunc(value, ctx.f32_type(), "wire.float.narrow")
                                .llvm_ctx("narrow decoded float")?
                                .into()
                        } else {
                            value.into()
                        }
                    }
                    _ => {
                        let value = if plan.ty == ResolvedTy::Char {
                            self.read("hew_cbor_de_char", ctx.i64_type().into(), &[])?
                        } else {
                            let signed = matches!(
                                plan.ty,
                                ResolvedTy::I8
                                    | ResolvedTy::I16
                                    | ResolvedTy::I32
                                    | ResolvedTy::I64
                                    | ResolvedTy::Isize
                                    | ResolvedTy::Duration
                            );
                            self.read(
                                "hew_cbor_de_int_checked",
                                ctx.i64_type().into(),
                                &[
                                    ctx.i32_type()
                                        .const_int(
                                            u64::from(ty.into_int_type().get_bit_width()),
                                            false,
                                        )
                                        .into(),
                                    ctx.i32_type().const_int(u64::from(signed), false).into(),
                                ],
                            )?
                        };
                        builder
                            .build_int_cast(
                                value.into_int_value(),
                                ty.into_int_type(),
                                "wire.integer.narrow",
                            )
                            .llvm_ctx("store range-checked wire integer")?
                            .into()
                    }
                };
                builder
                    .build_store(output.slot, value)
                    .llvm_ctx("stage decoded scalar")?;
                self.mark(output, true)
            }
            SemWireKind::Record { fields, .. } => {
                self.void("hew_cbor_de_enter_map", &[])?;
                self.check_cursor()?;
                let mut decoded = Vec::with_capacity(fields.len());
                for field in fields {
                    self.void(
                        if field.presence == WireFieldPresence::Optional {
                            "hew_cbor_de_select_optional_key"
                        } else {
                            "hew_cbor_de_select_key"
                        },
                        &[ctx.i64_type().const_int(u64::from(field.tag), false).into()],
                    )?;
                    decoded.push((field, self.child(&field.value)?));
                }
                self.void("hew_cbor_de_exit_map", &[])?;
                self.check_cursor()?;
                let layout =
                    self.values.module.target.layout(&plan.ty).ok_or_else(|| {
                        CodegenError::FailClosed("wire record has no layout".into())
                    })?;
                let mut record = llvm_type(ctx, &layout.repr)?
                    .into_struct_type()
                    .const_zero();
                for (field, value) in &decoded {
                    record = builder
                        .build_insert_value(
                            record,
                            self.load(value.slot, &field.value.ty)?,
                            field.index,
                            "wire.record.field",
                        )
                        .llvm_ctx("assemble complete wire record")?
                        .into_struct_value();
                }
                builder
                    .build_store(output.slot, record)
                    .llvm_ctx("stage complete wire record")?;
                self.mark(output, true)?;
                for (_, field) in decoded {
                    self.mark(field, false)?;
                }
                Ok(())
            }
            SemWireKind::Option {
                none, some, value, ..
            } => {
                let null = self
                    .read("hew_cbor_de_is_null", ctx.i32_type().into(), &[])?
                    .into_int_value();
                let absent = builder
                    .build_int_compare(
                        IntPredicate::NE,
                        null,
                        ctx.i32_type().const_zero(),
                        "wire.null",
                    )
                    .llvm_ctx("test wire Option null")?;
                let empty = ctx.append_basic_block(self.values.value, "wire.none");
                let present = ctx.append_basic_block(self.values.value, "wire.some");
                let done = ctx.append_basic_block(self.values.value, "wire.option.done");
                builder
                    .build_conditional_branch(absent, empty, present)
                    .llvm_ctx("decode Option presence")?;
                builder.position_at_end(empty);
                self.void("hew_cbor_de_skip", &[])?;
                self.variant(plan, *none, &[], output)?;
                builder
                    .build_unconditional_branch(done)
                    .llvm_ctx("finish None decode")?;
                builder.position_at_end(present);
                let field = self.child(value)?;
                self.variant(plan, *some, &[(field, value)], output)?;
                builder
                    .build_unconditional_branch(done)
                    .llvm_ctx("finish Some decode")?;
                builder.position_at_end(done);
                Ok(())
            }
            SemWireKind::Enum { variants, .. } => {
                let tag = self
                    .read("hew_cbor_de_enum_begin", ctx.i64_type().into(), &[])?
                    .into_int_value();
                self.check_cursor()?;
                let invalid = ctx.append_basic_block(self.values.value, "wire.unknown.tag");
                let done = ctx.append_basic_block(self.values.value, "wire.enum.done");
                let cases = variants
                    .iter()
                    .map(|variant| {
                        (
                            ctx.i64_type().const_int(u64::from(variant.tag), false),
                            ctx.append_basic_block(self.values.value, "wire.variant"),
                        )
                    })
                    .collect::<Vec<_>>();
                builder
                    .build_switch(tag, invalid, &cases)
                    .llvm_ctx("select checked wire tag")?;
                builder.position_at_end(invalid);
                self.fail_now()?;
                for (variant, (_, block)) in variants.iter().zip(cases) {
                    builder.position_at_end(block);
                    let mut fields = Vec::with_capacity(variant.fields.len());
                    for field in &variant.fields {
                        self.read("hew_cbor_de_array_next", ctx.i32_type().into(), &[])?;
                        fields.push((self.child(field)?, field.as_ref()));
                    }
                    self.void("hew_cbor_de_enum_end", &[])?;
                    self.check_cursor()?;
                    self.variant(plan, variant.index, &fields, output)?;
                    builder
                        .build_unconditional_branch(done)
                        .llvm_ctx("finish wire variant decode")?;
                }
                builder.position_at_end(done);
                Ok(())
            }
            SemWireKind::Vector(value) => self.collection(plan, value, None, output),
            SemWireKind::Set(value) => self.collection(plan, value, None, output),
            SemWireKind::Map { key, value } => self.collection(plan, key, Some(value), output),
        }
    }
    fn descriptor(&self, name: &str) -> CodegenResult<PointerValue<'ctx>> {
        self.values
            .llvm
            .get_global(name)
            .map(|global| global.as_pointer_value())
            .ok_or_else(|| {
                CodegenError::FailClosed(format!("wire collection descriptor `{name}` is absent"))
            })
    }
    fn collection(
        &mut self,
        plan: &SemWirePlan,
        key: &SemWirePlan,
        value: Option<&SemWirePlan>,
        output: DecodeTemporary<'ctx>,
    ) -> CodegenResult<()> {
        let ctx = self.values.ctx;
        let builder = self.values.builder;
        let pointer = ctx.ptr_type(AddressSpace::default());
        let map = value.is_some();
        let vector = matches!(plan.kind, SemWireKind::Vector(_));
        let collection = if vector {
            let glue = self
                .values
                .module
                .vector_glue
                .iter()
                .find(|glue| glue.ty == plan.ty)
                .ok_or_else(|| {
                    CodegenError::FailClosed("wire vector has no physical glue".into())
                })?;
            runtime_value(
                &self.values,
                "hew_vec_new_with_elem_layout",
                pointer.into(),
                &[self.descriptor(&vector_descriptor_symbol(glue.id))?.into()],
            )?
        } else if map {
            let glue = self
                .values
                .module
                .map_glue
                .iter()
                .find(|glue| glue.ty == plan.ty)
                .ok_or_else(|| CodegenError::FailClosed("wire map has no physical glue".into()))?;
            runtime_value(
                &self.values,
                "hew_hashmap_new_with_layout",
                pointer.into(),
                &[
                    self.descriptor(&map_key_descriptor_symbol(glue.id))?.into(),
                    self.descriptor(&map_value_descriptor_symbol(glue.id))?
                        .into(),
                ],
            )?
        } else {
            let glue = self
                .values
                .module
                .set_glue
                .iter()
                .find(|glue| glue.ty == plan.ty)
                .ok_or_else(|| CodegenError::FailClosed("wire set has no physical glue".into()))?;
            runtime_value(
                &self.values,
                "hew_hashset_new_with_layout",
                pointer.into(),
                &[self.descriptor(&set_key_descriptor_symbol(glue.id))?.into()],
            )?
        };
        builder
            .build_store(output.slot, collection)
            .llvm_ctx("stage owned decoded collection")?;
        self.mark(output, true)?;
        self.void(
            if map {
                "hew_cbor_de_enter_map_iter"
            } else {
                "hew_cbor_de_enter_array"
            },
            &[],
        )?;
        self.check_cursor()?;
        let key_slot = self.temporary(&key.ty)?;
        let value_slot = value.map(|plan| self.temporary(&plan.ty)).transpose()?;
        let inserted = self
            .values
            .entry_scratch(ctx.bool_type().into(), "wire.inserted")?;
        let header = ctx.append_basic_block(self.values.value, "wire.collection.next");
        let body = ctx.append_basic_block(self.values.value, "wire.collection.element");
        let done = ctx.append_basic_block(self.values.value, "wire.collection.done");
        builder
            .build_unconditional_branch(header)
            .llvm_ctx("enter collection decode loop")?;
        builder.position_at_end(header);
        let next = self
            .read(
                if map {
                    "hew_cbor_de_map_next"
                } else {
                    "hew_cbor_de_array_next"
                },
                ctx.i32_type().into(),
                &[],
            )?
            .into_int_value();
        let present = builder
            .build_int_compare(
                IntPredicate::NE,
                next,
                ctx.i32_type().const_zero(),
                "wire.element.present",
            )
            .llvm_ctx("check decoded element presence")?;
        builder
            .build_conditional_branch(present, body, done)
            .llvm_ctx("iterate decoded collection")?;
        builder.position_at_end(body);
        self.child_into(key, key_slot)?;
        if let (Some(value), Some(value_slot)) = (value, value_slot) {
            self.void("hew_cbor_de_map_value", &[])?;
            self.child_into(value, value_slot)?;
            let status = runtime_value(
                &self.values,
                "hew_hashmap_insert_take_layout",
                ctx.i32_type().into(),
                &[
                    collection,
                    key_slot.slot.into(),
                    value_slot.slot.into(),
                    inserted.into(),
                    self.fault.into(),
                ],
            )?
            .into_int_value();
            self.check_status(status)?;
            self.mark(value_slot, false)?;
            self.release(key_slot)?;
            let unique = builder
                .build_load(ctx.bool_type(), inserted, "wire.key.unique")
                .llvm_ctx("check decoded map key uniqueness")?
                .into_int_value();
            let accepted = ctx.append_basic_block(self.values.value, "wire.key.accepted");
            let duplicate = ctx.append_basic_block(self.values.value, "wire.key.duplicate");
            builder
                .build_conditional_branch(unique, accepted, duplicate)
                .llvm_ctx("reject repeated semantic map key")?;
            builder.position_at_end(duplicate);
            self.fail_now()?;
            builder.position_at_end(accepted);
        } else if vector {
            runtime(
                &self.values,
                "hew_vec_push_owned_move",
                None,
                &[collection, key_slot.slot.into()],
            )?;
            self.mark(key_slot, false)?;
        } else {
            let status = runtime_value(
                &self.values,
                "hew_hashset_insert_clone_layout",
                ctx.i32_type().into(),
                &[
                    collection,
                    key_slot.slot.into(),
                    inserted.into(),
                    self.fault.into(),
                ],
            )?
            .into_int_value();
            self.check_status(status)?;
            self.release(key_slot)?;
            let unique = builder
                .build_load(ctx.bool_type(), inserted, "wire.element.unique")
                .llvm_ctx("check decoded set element uniqueness")?
                .into_int_value();
            let accepted = ctx.append_basic_block(self.values.value, "wire.element.accepted");
            let duplicate = ctx.append_basic_block(self.values.value, "wire.element.duplicate");
            builder
                .build_conditional_branch(unique, accepted, duplicate)
                .llvm_ctx("reject duplicate set element")?;
            builder.position_at_end(duplicate);
            self.fail_now()?;
            builder.position_at_end(accepted);
        }
        builder
            .build_unconditional_branch(header)
            .llvm_ctx("continue decoded collection")?;
        builder.position_at_end(done);
        self.void(
            if map {
                "hew_cbor_de_exit_map_iter"
            } else {
                "hew_cbor_de_exit_array"
            },
            &[],
        )
    }
}

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    #[expect(
        clippy::too_many_arguments,
        reason = "the wire terminator supplies its schema, ownership recipes and exact result/fault edges"
    )]
    pub(super) fn emit_wire_codec(
        &self,
        direction: WireCodecDirection,
        plan: &SemWirePlan,
        recipes: &BTreeMap<ResolvedTy, PhysicalValueRecipe>,
        text_result: Option<hew_mir::physical::PhysicalWireTextResult>,
        input: ArgumentTransfer,
        result: StorageId,
        normal: &PhysicalEdge,
        unwind: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let ArgumentTransfer::Borrow(input) = input else {
            return Err(CodegenError::FailClosed(
                "wire input must remain borrowed".into(),
            ));
        };
        let values = self.value_emitter();
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let callback = emit_callback(
            self.module,
            self.ctx,
            self.llvm,
            plan,
            recipes,
            !direction.is_serialize(),
        )?;
        let schema = text_descriptor(plan, direction.text_format() == Some(WireTextFormat::Yaml))?
            .to_string();
        let descriptor = constant_text(&values, schema.as_bytes(), true);
        let format = match direction.text_format() {
            None => -1_i32,
            Some(WireTextFormat::Json) => 0,
            Some(WireTextFormat::Yaml) => 1,
        };
        let format = self.ctx.i32_type().const_int(format as u64, true).into();
        let error_out = values.entry_scratch(pointer.into(), "wire.error")?;
        self.builder
            .build_store(error_out, pointer.const_null())
            .llvm_ctx("initialize wire error owner")?;
        let success = self.ctx.append_basic_block(self.value, "wire.success");
        let rejected = self.ctx.append_basic_block(self.value, "wire.rejected");
        let decoded = if direction.is_serialize() {
            let writer = runtime_value(&values, "hew_cbor_ser_new", pointer.into(), &[])?;
            self.builder
                .build_call(
                    callback,
                    &[writer.into(), self.slots[input.0 as usize].into()],
                    "",
                )
                .llvm_ctx("encode borrowed wire value")?;
            let status = runtime_value(
                &values,
                "hew_wire_encode_finish",
                self.ctx.i32_type().into(),
                &[
                    writer,
                    format,
                    descriptor.into(),
                    self.slots[result.0 as usize].into(),
                    error_out.into(),
                ],
            )?
            .into_int_value();
            let complete = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    status,
                    self.ctx.i32_type().const_zero(),
                    "wire.encoded",
                )
                .llvm_ctx("test wire encoding result")?;
            self.builder
                .build_conditional_branch(complete, success, rejected)
                .llvm_ctx("publish successful wire encoding")?;
            None
        } else {
            let reader_out = values.entry_scratch(pointer.into(), "wire.reader")?;
            let fault_out = values.entry_scratch(pointer.into(), "wire.callback.fault")?;
            self.builder
                .build_store(fault_out, pointer.const_null())
                .llvm_ctx("initialize wire callback fault owner")?;
            let layout = self.module.target.layout(&plan.ty).ok_or_else(|| {
                CodegenError::FailClosed("wire decode output lacks layout".into())
            })?;
            let decoded =
                values.entry_scratch(llvm_type(self.ctx, &layout.repr)?, "wire.decoded.value")?;
            let prepared = runtime_value(
                &values,
                "hew_wire_decode_begin",
                self.ctx.i32_type().into(),
                &[
                    self.slots[input.0 as usize].into(),
                    format,
                    descriptor.into(),
                    reader_out.into(),
                    error_out.into(),
                ],
            )?
            .into_int_value();
            let ready = self.ctx.append_basic_block(self.value, "wire.decode.ready");
            let complete = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    prepared,
                    self.ctx.i32_type().const_zero(),
                    "wire.prepared",
                )
                .llvm_ctx("check wire decode input")?;
            self.builder
                .build_conditional_branch(complete, ready, rejected)
                .llvm_ctx("enter prepared wire reader")?;
            self.builder.position_at_end(ready);
            let reader = self
                .builder
                .build_load(pointer, reader_out, "wire.reader")
                .llvm_ctx("load owned wire reader")?;
            let status = self
                .runtime_call_value(
                    callback,
                    &[reader.into(), decoded.into(), fault_out.into()],
                    "wire.decode.status",
                )?
                .into_int_value();
            runtime(&values, "hew_cbor_de_free", None, &[reader])?;
            let failed = self
                .ctx
                .append_basic_block(self.value, "wire.decode.failed");
            let complete = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    status,
                    self.ctx.i32_type().const_zero(),
                    "wire.decoded",
                )
                .llvm_ctx("test complete wire decode")?;
            self.builder
                .build_conditional_branch(complete, success, failed)
                .llvm_ctx("publish complete decoded value")?;
            self.builder.position_at_end(failed);
            let fault = self
                .builder
                .build_load(pointer, fault_out, "wire.callback.fault")
                .llvm_ctx("read collection callback fault")?;
            let fault_present = self
                .builder
                .build_is_not_null(fault.into_pointer_value(), "wire.callback.failed")
                .llvm_ctx("distinguish callback failure from malformed input")?;
            let propagate = self
                .ctx
                .append_basic_block(self.value, "wire.callback.propagate");
            let malformed = self.ctx.append_basic_block(self.value, "wire.malformed");
            self.builder
                .build_conditional_branch(fault_present, propagate, malformed)
                .llvm_ctx("preserve wire collection fault identity")?;
            self.builder.position_at_end(propagate);
            self.store_active_fault_value(fault, status)?;
            self.emit_edge(unwind)?;
            self.builder.position_at_end(malformed);
            let message = b"wire body does not match the expected value type";
            let data = constant_text(&values, message, false);
            runtime(
                &values,
                "hew_string_literal_new",
                None,
                &[
                    data.into(),
                    self.ctx
                        .i32_type()
                        .const_int(message.len() as u64, false)
                        .into(),
                    error_out.into(),
                ],
            )?;
            self.builder
                .build_unconditional_branch(rejected)
                .llvm_ctx("return malformed wire error")?;
            Some((decoded, llvm_type(self.ctx, &layout.repr)?))
        };
        self.builder.position_at_end(rejected);
        let error = self
            .builder
            .build_load(pointer, error_out, "wire.error.message")
            .llvm_ctx("take wire error string")?;
        if let Some(cases) = text_result {
            self.write_variant_value(
                self.slots[result.0 as usize],
                cases.error,
                &[error],
                cases.glue,
            )?;
            self.emit_result_edge(Some(result), normal)?;
        } else {
            let (fault, code) = if direction == WireCodecDirection::Decode {
                let code = hew_runtime::internal::types::HEW_TRAP_WIRE_DECODE_FAILED;
                let constructor = external_fault_new(self.ctx, self.llvm)?;
                (
                    self.runtime_call_value(
                        constructor,
                        &[self.ctx.i32_type().const_int(code as u64, true).into()],
                        "wire.decode.fault",
                    )?,
                    code,
                )
            } else {
                (
                    runtime_value(&values, "hew_fault_new_panic", pointer.into(), &[error])?,
                    HEW_TRAP_USER_PANIC,
                )
            };
            runtime(&values, "hew_string_drop", None, &[error])?;
            self.store_active_fault(fault, code)?;
            self.emit_edge(unwind)?;
        }
        self.builder.position_at_end(success);
        if let Some((decoded, ty)) = decoded {
            let value = self
                .builder
                .build_load(ty, decoded, "wire.complete.value")
                .llvm_ctx("take decoded value owner")?;
            if let Some(cases) = text_result {
                self.write_variant_value(
                    self.slots[result.0 as usize],
                    cases.ok,
                    &[value],
                    cases.glue,
                )?;
            } else {
                self.store(result, value)?;
            }
        }
        self.emit_result_edge(Some(result), normal)
    }
}
