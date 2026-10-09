//! Native codec walks: one encode and one decode callback per planned type,
//! driving the format-neutral `hew_ser_*` / `hew_de_*` event ABI over static
//! `hew_codec::Table` globals.

use super::collection_callbacks::CollectionCallbacks;
use super::*;
use hew_mir::physical::{SemWireKind, SemWirePayload, SemWirePlan, SemWirePlans, SemWireTable};
use hew_types::Codec;

fn wire_symbol(ty: &ResolvedTy, decode: bool) -> String {
    format!(
        "__hew_wire_{}_{}",
        if decode { "decode" } else { "encode" },
        hew_types::mangle_resolved_ty(ty)
    )
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
    // The codec runtime takes every Boolean and flag as a C `i8`.
    let widen = (0_u32..)
        .zip(args)
        .filter(|(_, arg)| arg.get_type() == values.ctx.i8_type().into())
        .map(|(index, _)| (index, Widen::Sign))
        .collect::<Vec<_>>();
    let function = get_or_declare_external_widened(values.llvm, name, signature, &widen)?;
    let args = args.iter().copied().map(Into::into).collect::<Vec<_>>();
    let call = values
        .builder
        .build_call(
            function,
            &args,
            if result.is_some() { "wire.runtime" } else { "" },
        )
        .llvm_ctx("call codec runtime")?;
    Ok(call.try_as_basic_value().basic())
}

fn runtime_value<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    name: &str,
    result: BasicTypeEnum<'ctx>,
    args: &[BasicValueEnum<'ctx>],
) -> CodegenResult<BasicValueEnum<'ctx>> {
    runtime(values, name, Some(result), args)?
        .ok_or_else(|| CodegenError::FailClosed("codec runtime returned no value".into()))
}

fn private_constant<'ctx>(
    llvm: &Module<'ctx>,
    name: &str,
    value: BasicValueEnum<'ctx>,
) -> PointerValue<'ctx> {
    let global = llvm.add_global(value.get_type(), None, name);
    global.set_linkage(Linkage::Private);
    global.set_constant(true);
    global.set_initializer(&value);
    global.as_pointer_value()
}

/// The `hew_codec::Table` global for `table`, emitted once per symbol: a
/// pointer and count of `Member { key_ptr, key_len, tag: u64, flags: u32 }`
/// records and a one-byte tagged flag, with pointer-sized lengths.
fn table_global<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    symbol: &str,
    table: &SemWireTable,
) -> PointerValue<'ctx> {
    if let Some(global) = values.llvm.get_global(symbol) {
        return global.as_pointer_value();
    }
    let ctx = values.ctx;
    let target = TargetData::create(&values.module.target.data_layout);
    let word = ctx.ptr_sized_int_type(&target, None);
    let pointer = ctx.ptr_type(AddressSpace::default());
    let member_ty = ctx.struct_type(
        &[
            pointer.into(),
            word.into(),
            ctx.i64_type().into(),
            ctx.i32_type().into(),
        ],
        false,
    );
    let members = table
        .members
        .iter()
        .enumerate()
        .map(|(index, member)| {
            let key = private_constant(
                values.llvm,
                &format!("{symbol}.key{index}"),
                ctx.const_string(member.key.as_bytes(), false).into(),
            );
            member_ty.const_named_struct(&[
                key.into(),
                word.const_int(member.key.len() as u64, false).into(),
                ctx.i64_type().const_int(member.tag, false).into(),
                ctx.i32_type()
                    .const_int(u64::from(member.flags), false)
                    .into(),
            ])
        })
        .collect::<Vec<_>>();
    let members = private_constant(
        values.llvm,
        &format!("{symbol}.members"),
        member_ty.const_array(&members).into(),
    );
    let table_ty = ctx.struct_type(&[pointer.into(), word.into(), ctx.i8_type().into()], false);
    private_constant(
        values.llvm,
        symbol,
        table_ty
            .const_named_struct(&[
                members.into(),
                word.const_int(table.members.len() as u64, false).into(),
                ctx.i8_type()
                    .const_int(u64::from(table.tagged), false)
                    .into(),
            ])
            .into(),
    )
}

fn table_symbol(ty: &ResolvedTy, variant: Option<usize>) -> String {
    let base = format!("__hew_wire_table_{}", hew_types::mangle_resolved_ty(ty));
    variant.map_or(base.clone(), |index| format!("{base}_v{index}"))
}

/// Whether decoding `ty` can suspend: a key capability or a destructor of a
/// type its walk reaches resumes. Decided per type so every plan set that
/// reaches it agrees on its callback signature.
fn decode_is_resumable(
    module: &PhysicalModule,
    plans: &SemWirePlans,
    ty: &ResolvedTy,
    recipes: &BTreeMap<ResolvedTy, PhysicalValueRecipe>,
) -> bool {
    let mut seen = BTreeSet::new();
    let mut work = vec![ty];
    while let Some(ty) = work.pop() {
        if !seen.insert(ty) {
            continue;
        }
        let Some(plan) = plans.get(ty) else {
            continue;
        };
        if recipes[ty]
            .destroy
            .is_some_and(|action| module.releases.suspends(action))
        {
            return true;
        }
        if let SemWireKind::Set(key) | SemWireKind::Map { key, .. } = &plan.kind {
            if [
                hew_types::ValueCapability::Hash,
                hew_types::ValueCapability::Eq,
            ]
            .into_iter()
            .any(|capability| module.value_capabilities[&(key.clone(), capability)].is_resumable)
            {
                return true;
            }
        }
        work.extend(plan.children());
    }
    false
}

/// The encode or decode callback of `ty`, emitted once per type.
#[expect(
    clippy::too_many_arguments,
    reason = "a callback needs its plan set, value recipes and selected key capabilities"
)]
pub(super) fn emit_callback<'ctx>(
    module: &PhysicalModule,
    ctx: &'ctx Context,
    llvm: &Module<'ctx>,
    plans: &SemWirePlans,
    ty: &ResolvedTy,
    recipes: &BTreeMap<ResolvedTy, PhysicalValueRecipe>,
    callbacks: &key::CallbackTable<'ctx>,
    decode: bool,
) -> CodegenResult<FunctionValue<'ctx>> {
    let name = wire_symbol(ty, decode);
    if let Some(function) = llvm.get_function(&name) {
        return Ok(function);
    }
    let plan = plans
        .get(ty)
        .ok_or_else(|| CodegenError::FailClosed("codec plan set lacks a reached type".into()))?;
    let pointer = ctx.ptr_type(AddressSpace::default());
    let resumable = decode && decode_is_resumable(module, plans, ty, recipes);
    let signature = if resumable {
        pointer.fn_type(&[pointer.into(); 4], false)
    } else if decode {
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
        .llvm_ctx("enter codec callback")?;
    builder.position_at_end(body);
    let frame = if resumable {
        Some(coro::begin(
            ctx,
            llvm,
            &builder,
            function,
            function.get_nth_param(3).unwrap().into_pointer_value(),
        )?)
    } else {
        None
    };
    let allocations = builder
        .get_insert_block()
        .expect("codec callback body exists");
    let values = ValueEmitter {
        module,
        ctx,
        llvm,
        builder: &builder,
        value: function,
        fault_sink: None,
    };
    let cursor = function
        .get_nth_param(0)
        .expect("codec callback cursor")
        .into_pointer_value();
    let slot = function
        .get_nth_param(1)
        .expect("codec callback value")
        .into_pointer_value();
    if decode {
        let fault = function
            .get_nth_param(2)
            .expect("codec callback fault output")
            .into_pointer_value();
        let fail = ctx.append_basic_block(function, "rollback");
        let status = builder
            .build_alloca(ctx.i32_type(), "wire.status")
            .llvm_ctx("allocate codec callback status")?;
        builder
            .build_store(status, ctx.i32_type().const_zero())
            .llvm_ctx("initialize decode failure status")?;
        let values = ValueEmitter {
            fault_sink: Some((fault, status)),
            ..values
        };
        let mut emitter = DecodeEmitter {
            values,
            plans,
            recipes,
            callbacks,
            frame,
            allocations,
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
        emitter.finish(ctx.i32_type().const_zero())?;
        emitter.rollback()?;
    } else {
        EncodeEmitter {
            values,
            plans,
            recipes,
            callbacks,
            cursor,
        }
        .encode(plan, slot)?;
        builder
            .build_return(None)
            .llvm_ctx("finish codec encode callback")?;
    }
    Ok(function)
}

fn is_signed(ty: &ResolvedTy) -> bool {
    matches!(
        ty,
        ResolvedTy::I8
            | ResolvedTy::I16
            | ResolvedTy::I32
            | ResolvedTy::I64
            | ResolvedTy::Isize
            | ResolvedTy::Duration
    )
}

fn index(value: usize) -> CodegenResult<u32> {
    u32::try_from(value).map_err(|_| CodegenError::FailClosed("codec index exceeds u32".into()))
}

struct EncodeEmitter<'a, 'ctx> {
    values: ValueEmitter<'a, 'ctx>,
    plans: &'a SemWirePlans,
    recipes: &'a BTreeMap<ResolvedTy, PhysicalValueRecipe>,
    callbacks: &'a key::CallbackTable<'ctx>,
    cursor: PointerValue<'ctx>,
}

impl<'ctx> EncodeEmitter<'_, 'ctx> {
    fn void(&self, name: &str, args: &[BasicValueEnum<'ctx>]) -> CodegenResult<()> {
        let args = std::iter::once(self.cursor.into())
            .chain(args.iter().copied())
            .collect::<Vec<_>>();
        runtime(&self.values, name, None, &args).map(|_| ())
    }
    fn len(&self, len: usize) -> BasicValueEnum<'ctx> {
        self.values
            .ctx
            .i64_type()
            .const_int(len as u64, false)
            .into()
    }
    fn table(
        &self,
        ty: &ResolvedTy,
        variant: Option<usize>,
        table: &SemWireTable,
    ) -> BasicValueEnum<'ctx> {
        table_global(&self.values, &table_symbol(ty, variant), table).into()
    }
    fn child(&self, ty: &ResolvedTy, slot: PointerValue<'ctx>) -> CodegenResult<()> {
        let function = emit_callback(
            self.values.module,
            self.values.ctx,
            self.values.llvm,
            self.plans,
            ty,
            self.recipes,
            self.callbacks,
            false,
        )?;
        self.values
            .builder
            .build_call(function, &[self.cursor.into(), slot.into()], "")
            .llvm_ctx("encode planned child")?;
        Ok(())
    }
    fn load(
        &self,
        slot: PointerValue<'ctx>,
        ty: &ResolvedTy,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let layout =
            self.values.module.target.layout(ty).ok_or_else(|| {
                CodegenError::FailClosed("codec type has no physical layout".into())
            })?;
        self.values
            .builder
            .build_load(
                llvm_type(self.values.ctx, &layout.repr)?,
                slot,
                "wire.value",
            )
            .llvm_ctx("read borrowed codec value")
    }
    fn field(
        &self,
        slot: PointerValue<'ctx>,
        ty: &ResolvedTy,
        field: u32,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let layout = self
            .values
            .module
            .target
            .layout(ty)
            .ok_or_else(|| CodegenError::FailClosed("codec aggregate has no layout".into()))?;
        self.values
            .builder
            .build_struct_gep(
                llvm_type(self.values.ctx, &layout.repr)?.into_struct_type(),
                slot,
                field,
                "wire.field",
            )
            .llvm_ctx("address codec aggregate field")
    }
    fn sequence(&self, slots: &[(PointerValue<'ctx>, &ResolvedTy)]) -> CodegenResult<()> {
        self.void("hew_ser_seq_begin", &[self.len(slots.len())])?;
        for (slot, ty) in slots {
            self.child(ty, *slot)?;
        }
        self.void("hew_ser_seq_end", &[])
    }
    fn record(
        &self,
        table: BasicValueEnum<'ctx>,
        members: &SemWireTable,
        slots: &[(PointerValue<'ctx>, &ResolvedTy)],
    ) -> CodegenResult<()> {
        self.void("hew_ser_record_begin", &[table])?;
        for (position, ((slot, ty), member)) in slots.iter().zip(&members.members).enumerate() {
            if member.flags & hew_codec::Member::SKIP != 0 {
                continue;
            }
            self.void(
                "hew_ser_field",
                &[self
                    .values
                    .ctx
                    .i32_type()
                    .const_int(u64::from(index(position)?), false)
                    .into()],
            )?;
            self.child(ty, *slot)?;
        }
        self.void("hew_ser_record_end", &[])
    }
    #[expect(
        clippy::too_many_lines,
        reason = "one exhaustive match keeps every planned shape's encoding together"
    )]
    fn encode(&self, plan: &SemWirePlan, slot: PointerValue<'ctx>) -> CodegenResult<()> {
        let ctx = self.values.ctx;
        let builder = self.values.builder;
        match &plan.kind {
            SemWireKind::Scalar => {
                match &plan.ty {
                    ResolvedTy::Bytes => return self.void("hew_ser_bytes", &[slot.into()]),
                    ResolvedTy::String => {
                        let value = self.load(slot, &plan.ty)?;
                        return self.void("hew_ser_str", &[value]);
                    }
                    _ => {}
                }
                let value = self.load(slot, &plan.ty)?;
                let (name, value) = match &plan.ty {
                    ResolvedTy::Bool => ("hew_ser_bool", value),
                    ResolvedTy::F32 => (
                        "hew_ser_f64",
                        builder
                            .build_float_ext(value.into_float_value(), ctx.f64_type(), "wire.float")
                            .llvm_ctx("widen codec float")?
                            .into(),
                    ),
                    ResolvedTy::F64 => ("hew_ser_f64", value),
                    ResolvedTy::Char => (
                        "hew_ser_char",
                        builder
                            .build_int_cast(value.into_int_value(), ctx.i32_type(), "wire.char")
                            .llvm_ctx("read codec char")?
                            .into(),
                    ),
                    ty => {
                        let signed = is_signed(ty);
                        let wide = builder
                            .build_int_cast_sign_flag(
                                value.into_int_value(),
                                ctx.i64_type(),
                                signed,
                                "wire.integer",
                            )
                            .llvm_ctx("widen codec integer")?;
                        (
                            if signed { "hew_ser_i64" } else { "hew_ser_u64" },
                            wide.into(),
                        )
                    }
                };
                self.void(name, &[value])
            }
            SemWireKind::Unit => self.void("hew_ser_null", &[]),
            SemWireKind::Tuple(elements) => {
                let slots = elements
                    .iter()
                    .enumerate()
                    .map(|(position, ty)| Ok((self.field(slot, &plan.ty, index(position)?)?, ty)))
                    .collect::<CodegenResult<Vec<_>>>()?;
                self.sequence(&slots)
            }
            SemWireKind::Array { element, len } => {
                let layout =
                    self.values.module.target.layout(&plan.ty).ok_or_else(|| {
                        CodegenError::FailClosed("codec array has no layout".into())
                    })?;
                let array_ty = llvm_type(ctx, &layout.repr)?;
                self.void(
                    "hew_ser_seq_begin",
                    &[ctx.i64_type().const_int(*len, false).into()],
                )?;
                self.counted(*len, |position| {
                    // SAFETY: the loop bound is the array's static length.
                    let element_slot = unsafe {
                        builder.build_in_bounds_gep(
                            array_ty,
                            slot,
                            &[ctx.i64_type().const_zero(), position],
                            "wire.element",
                        )
                    }
                    .llvm_ctx("address codec array element")?;
                    self.child(element, element_slot)
                })?;
                self.void("hew_ser_seq_end", &[])
            }
            SemWireKind::Record { table, fields, .. } => {
                let slots = fields
                    .iter()
                    .enumerate()
                    .map(|(position, ty)| Ok((self.field(slot, &plan.ty, index(position)?)?, ty)))
                    .collect::<CodegenResult<Vec<_>>>()?;
                match table {
                    Some(members) => {
                        self.record(self.table(&plan.ty, None, members), members, &slots)
                    }
                    None => self.sequence(&slots),
                }
            }
            SemWireKind::Option {
                none, some, value, ..
            } => {
                let layout = self.values.variant_layout(&plan.ty)?;
                let object = self.values.variant_object_ptr(slot, layout)?;
                let tag = self.values.load_variant_tag(object, layout)?;
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
                self.void("hew_ser_null", &[])?;
                builder
                    .build_unconditional_branch(done)
                    .llvm_ctx("finish None encoding")?;
                builder.position_at_end(present);
                self.child(
                    value,
                    variant_field_pointer(&self.values, object, &plan.ty, *some, 0)?,
                )?;
                builder
                    .build_unconditional_branch(done)
                    .llvm_ctx("finish Some encoding")?;
                builder.position_at_end(done);
                Ok(())
            }
            SemWireKind::Enum {
                table, variants, ..
            } => {
                let layout = self.values.variant_layout(&plan.ty)?;
                let object = self.values.variant_object_ptr(slot, layout)?;
                let tag = self.values.load_variant_tag(object, layout)?;
                let invalid = ctx.append_basic_block(self.values.value, "wire.invalid.enum");
                let done = ctx.append_basic_block(self.values.value, "wire.enum.done");
                let cases = (0..variants.len())
                    .map(|position| {
                        Ok((
                            tag.get_type().const_int(u64::from(index(position)?), false),
                            ctx.append_basic_block(self.values.value, "wire.variant"),
                        ))
                    })
                    .collect::<CodegenResult<Vec<_>>>()?;
                builder
                    .build_switch(tag, invalid, &cases)
                    .llvm_ctx("select codec enum variant")?;
                builder.position_at_end(invalid);
                self.values.emit_invalid_variant_tag()?;
                let enum_table = self.table(&plan.ty, None, table);
                for (position, (payload, (_, block))) in variants.iter().zip(cases).enumerate() {
                    builder.position_at_end(block);
                    let variant = index(position)?;
                    self.void(
                        "hew_ser_variant",
                        &[
                            enum_table,
                            ctx.i32_type().const_int(u64::from(variant), false).into(),
                        ],
                    )?;
                    let slots = payload
                        .types()
                        .iter()
                        .enumerate()
                        .map(|(field, ty)| {
                            Ok((
                                variant_field_pointer(
                                    &self.values,
                                    object,
                                    &plan.ty,
                                    variant,
                                    index(field)?,
                                )?,
                                ty,
                            ))
                        })
                        .collect::<CodegenResult<Vec<_>>>()?;
                    match payload {
                        SemWirePayload::Unit => {}
                        SemWirePayload::Single(ty) => self.child(ty, slots[0].0)?,
                        SemWirePayload::Tuple(_) => self.sequence(&slots)?,
                        SemWirePayload::Record { table, .. } => {
                            self.record(self.table(&plan.ty, Some(position), table), table, &slots)?
                        }
                    }
                    self.void("hew_ser_variant_end", &[])?;
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
    /// Run `body` for each position below `len`.
    fn counted(
        &self,
        len: u64,
        body: impl Fn(IntValue<'ctx>) -> CodegenResult<()>,
    ) -> CodegenResult<()> {
        counted_loop(
            &self.values,
            self.values.ctx.i64_type().const_int(len, false),
            body,
        )
    }
    fn vector(
        &self,
        plan: &SemWirePlan,
        element: &ResolvedTy,
        slot: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let ctx = self.values.ctx;
        let vector = self.load(slot, &plan.ty)?;
        let len = runtime_value(
            &self.values,
            "hew_vec_len",
            ctx.i64_type().into(),
            &[vector],
        )?
        .into_int_value();
        self.void("hew_ser_seq_begin", &[len.into()])?;
        counted_loop(&self.values, len, |position| {
            let ptr = runtime_value(
                &self.values,
                "hew_vec_get_owned",
                ctx.ptr_type(AddressSpace::default()).into(),
                &[vector, position.into()],
            )?
            .into_pointer_value();
            self.child(element, ptr)
        })?;
        self.void("hew_ser_seq_end", &[])
    }
    fn associative(
        &self,
        plan: &SemWirePlan,
        key: &ResolvedTy,
        value: Option<&ResolvedTy>,
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
        if map {
            let string_keys = u64::from(*key == ResolvedTy::String);
            self.void(
                "hew_ser_map_begin",
                &[
                    self.len(0),
                    ctx.i8_type().const_int(string_keys, false).into(),
                ],
            )?;
        } else {
            self.void("hew_ser_set_begin", &[self.len(0)])?;
        }
        let header = ctx.append_basic_block(self.values.value, "wire.collection.next");
        let body = ctx.append_basic_block(self.values.value, "wire.collection.element");
        let done = ctx.append_basic_block(self.values.value, "wire.collection.done");
        builder
            .build_unconditional_branch(header)
            .llvm_ctx("enter codec collection loop")?;
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
            .llvm_ctx("advance codec collection cursor")?;
        builder.position_at_end(body);
        let key_slot = builder
            .build_load(pointer, key_out, "wire.key")
            .llvm_ctx("read borrowed codec key")?
            .into_pointer_value();
        self.child(key, key_slot)?;
        if let Some(value) = value {
            let value_slot = builder
                .build_load(pointer, value_out, "wire.value")
                .llvm_ctx("read borrowed codec map value")?
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
                "hew_ser_map_end"
            } else {
                "hew_ser_seq_end"
            },
            &[],
        )
    }
}

/// Emit `for position in 0..len { body(position) }`.
fn counted_loop<'ctx>(
    values: &ValueEmitter<'_, 'ctx>,
    len: IntValue<'ctx>,
    body: impl Fn(IntValue<'ctx>) -> CodegenResult<()>,
) -> CodegenResult<()> {
    let ctx = values.ctx;
    let builder = values.builder;
    let counter = values.entry_scratch(ctx.i64_type().into(), "wire.index")?;
    builder
        .build_store(counter, ctx.i64_type().const_zero())
        .llvm_ctx("initialize codec index")?;
    let header = ctx.append_basic_block(values.value, "wire.loop.next");
    let inner = ctx.append_basic_block(values.value, "wire.loop.body");
    let done = ctx.append_basic_block(values.value, "wire.loop.done");
    builder
        .build_unconditional_branch(header)
        .llvm_ctx("enter codec loop")?;
    builder.position_at_end(header);
    let position = builder
        .build_load(ctx.i64_type(), counter, "wire.index")
        .llvm_ctx("read codec index")?
        .into_int_value();
    let more = builder
        .build_int_compare(IntPredicate::ULT, position, len, "wire.more")
        .llvm_ctx("check codec loop bound")?;
    builder
        .build_conditional_branch(more, inner, done)
        .llvm_ctx("iterate codec loop")?;
    builder.position_at_end(inner);
    body(position)?;
    let next = builder
        .build_int_add(
            position,
            ctx.i64_type().const_int(1, false),
            "wire.index.next",
        )
        .llvm_ctx("advance codec index")?;
    builder
        .build_store(counter, next)
        .llvm_ctx("store codec index")?;
    builder
        .build_unconditional_branch(header)
        .llvm_ctx("continue codec loop")?;
    builder.position_at_end(done);
    Ok(())
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
        .ok_or_else(|| CodegenError::FailClosed("codec variant has no physical payload".into()))?;
    values
        .builder
        .build_struct_gep(
            llvm_type(values.ctx, &layout.repr)?.into_struct_type(),
            payload,
            field,
            "wire.payload.field",
        )
        .llvm_ctx("address checked codec payload field")
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
    plans: &'a SemWirePlans,
    recipes: &'a BTreeMap<ResolvedTy, PhysicalValueRecipe>,
    callbacks: &'a key::CallbackTable<'ctx>,
    frame: Option<coro::Frame<'ctx>>,
    allocations: BasicBlock<'ctx>,
    cursor: PointerValue<'ctx>,
    fault: PointerValue<'ctx>,
    fail: BasicBlock<'ctx>,
    status: PointerValue<'ctx>,
    owners: Vec<DecodeOwner<'ctx>>,
}

impl<'ctx> DecodeEmitter<'_, 'ctx> {
    fn scratch(&self, ty: BasicTypeEnum<'ctx>, name: &str) -> CodegenResult<PointerValue<'ctx>> {
        let builder = self.values.ctx.create_builder();
        if let Some(end) = self.allocations.get_terminator() {
            builder.position_before(&end);
        } else {
            builder.position_at_end(self.allocations);
        }
        builder
            .build_alloca(ty, name)
            .llvm_ctx("allocate reusable codec callback scratch")
    }
    fn finish(&self, status: IntValue<'ctx>) -> CodegenResult<()> {
        if let Some(frame) = &self.frame {
            let pointer = self.values.ctx.ptr_type(AddressSpace::default());
            let finish = get_or_declare_external(
                self.values.llvm,
                "hew_coro_state_finish",
                self.values
                    .ctx
                    .i32_type()
                    .fn_type(&[pointer.into(), self.values.ctx.i32_type().into()], false),
            )?;
            self.values
                .builder
                .build_call(finish, &[frame.state.into(), status.into()], "")
                .llvm_ctx("publish codec decode outcome")?;
            self.values
                .builder
                .build_unconditional_branch(frame.finish)
                .llvm_ctx("finish codec decoder frame")?;
        } else {
            self.values
                .builder
                .build_return(Some(&status))
                .llvm_ctx("return codec decode status")?;
        }
        Ok(())
    }
    fn temporary(&mut self, ty: &ResolvedTy) -> CodegenResult<DecodeTemporary<'ctx>> {
        let recipe = self.recipes.get(ty).ok_or_else(|| {
            CodegenError::FailClosed("codec decode lacks exact value recipe".into())
        })?;
        let layout = self.values.module.target.layout(ty).ok_or_else(|| {
            CodegenError::FailClosed("codec decode lacks physical type layout".into())
        })?;
        let slot = self.scratch(llvm_type(self.values.ctx, &layout.repr)?, "wire.temporary")?;
        let owner = if recipe.destroy.is_some() {
            let initialized =
                self.scratch(self.values.ctx.bool_type().into(), "wire.initialized")?;
            let prologue = self.allocations;
            let builder = self.values.ctx.create_builder();
            if let Some(end) = prologue.get_terminator() {
                builder.position_before(&end);
            } else {
                builder.position_at_end(prologue);
            }
            builder
                .build_store(initialized, self.values.ctx.bool_type().const_zero())
                .llvm_ctx("initialize codec cleanup obligation")?;
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
                .llvm_ctx("transfer codec cleanup obligation")?;
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
        self.values
            .builder
            .build_store(owner.initialized, self.values.ctx.bool_type().const_zero())
            .llvm_ctx("consume codec cleanup owner")?;
        let layout = self
            .values
            .module
            .target
            .layout(&owner.recipe.ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("codec cleanup has no physical layout".into())
            })?;
        let action = owner
            .recipe
            .destroy
            .expect("owned temporary has destruction");
        if let Some(frame) = &self.frame {
            return release::slot(&self.values, frame, owner.slot, layout, action);
        }
        let value = self
            .values
            .builder
            .build_load(
                llvm_type(self.values.ctx, &layout.repr)?,
                owner.slot,
                "wire.partial.owner",
            )
            .llvm_ctx("read initialized codec owner")?;
        self.values.destroy_loaded_value(value, layout, action)
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
                .llvm_ctx("test partial codec owner")?
                .into_int_value();
            self.values
                .builder
                .build_conditional_branch(initialized, release, next)
                .llvm_ctx("release only initialized codec fields")?;
            self.values.builder.position_at_end(release);
            self.destroy(owner)?;
            self.values
                .builder
                .build_unconditional_branch(next)
                .llvm_ctx("continue codec rollback")?;
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
            .llvm_ctx("read codec failure status")?;
        self.finish(status.into_int_value())
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
            .ok_or_else(|| CodegenError::FailClosed("codec value has no layout".into()))?;
        self.values
            .builder
            .build_load(
                llvm_type(self.values.ctx, &layout.repr)?,
                slot,
                "wire.decoded",
            )
            .llvm_ctx("read complete codec value")
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
    fn i32(&self, value: u64) -> BasicValueEnum<'ctx> {
        self.values.ctx.i32_type().const_int(value, false).into()
    }
    fn table(
        &self,
        ty: &ResolvedTy,
        variant: Option<usize>,
        table: &SemWireTable,
    ) -> BasicValueEnum<'ctx> {
        table_global(&self.values, &table_symbol(ty, variant), table).into()
    }
    fn check_status(&self, status: IntValue<'ctx>) -> CodegenResult<()> {
        let success = self
            .values
            .ctx
            .append_basic_block(self.values.value, "wire.checked");
        self.values
            .builder
            .build_store(self.status, status)
            .llvm_ctx("retain codec failure status")?;
        let failed = self
            .values
            .builder
            .build_int_compare(
                IntPredicate::NE,
                status,
                status.get_type().const_zero(),
                "wire.failed",
            )
            .llvm_ctx("test codec callback status")?;
        self.values
            .builder
            .build_conditional_branch(failed, self.fail, success)
            .llvm_ctx("reject incomplete codec value")?;
        self.values.builder.position_at_end(success);
        Ok(())
    }
    fn check_cursor(&self) -> CodegenResult<()> {
        let status = self
            .read("hew_de_failed", self.values.ctx.i32_type().into(), &[])?
            .into_int_value();
        self.check_status(status)
    }
    fn fail_now(&self) -> CodegenResult<()> {
        self.values
            .builder
            .build_store(self.status, self.values.ctx.i32_type().const_int(1, false))
            .llvm_ctx("record malformed codec value")?;
        self.values
            .builder
            .build_unconditional_branch(self.fail)
            .llvm_ctx("reject malformed codec value")?;
        Ok(())
    }
    fn child_into(&self, ty: &ResolvedTy, temporary: DecodeTemporary<'ctx>) -> CodegenResult<()> {
        let function = emit_callback(
            self.values.module,
            self.values.ctx,
            self.values.llvm,
            self.plans,
            ty,
            self.recipes,
            self.callbacks,
            true,
        )?;
        let arguments = [self.cursor.into(), temporary.slot.into(), self.fault.into()];
        let status = if decode_is_resumable(self.values.module, self.plans, ty, self.recipes) {
            suspend::invoke_child(
                self.values.ctx,
                self.values.llvm,
                self.values.builder,
                self.values.value,
                self.frame.as_ref().ok_or_else(|| {
                    CodegenError::FailClosed("suspending codec child lacks a caller frame".into())
                })?,
                function,
                &arguments,
            )?
        } else {
            self.values
                .builder
                .build_call(function, &arguments, "wire.child.status")
                .llvm_ctx("decode exact codec child")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed("codec decoder returned no status".into()))?
                .into_int_value()
        };
        self.check_status(status)?;
        self.mark(temporary, true)
    }
    fn child(&mut self, ty: &ResolvedTy) -> CodegenResult<DecodeTemporary<'ctx>> {
        let temporary = self.temporary(ty)?;
        self.child_into(ty, temporary)?;
        Ok(temporary)
    }
    fn variant(
        &self,
        plan: &SemWirePlan,
        variant: u32,
        fields: &[(DecodeTemporary<'ctx>, &ResolvedTy)],
        output: DecodeTemporary<'ctx>,
    ) -> CodegenResult<()> {
        let glue = self
            .values
            .module
            .variant_glue
            .iter()
            .find(|glue| glue.ty == plan.ty)
            .ok_or_else(|| {
                CodegenError::FailClosed("codec variant has no exact physical glue".into())
            })?;
        let values = fields
            .iter()
            .map(|(value, ty)| self.load(value.slot, ty))
            .collect::<CodegenResult<Vec<_>>>()?;
        self.values
            .write_variant_value(output.slot, variant, &values, glue.id)?;
        self.mark(output, true)?;
        for (field, _) in fields {
            self.mark(*field, false)?;
        }
        Ok(())
    }
    /// Decode a sequence of exactly these types, each into a temporary.
    fn sequence<'t>(
        &mut self,
        types: &'t [ResolvedTy],
    ) -> CodegenResult<Vec<(DecodeTemporary<'ctx>, &'t ResolvedTy)>> {
        self.void("hew_de_tuple_begin", &[self.i32(types.len() as u64)])?;
        let mut decoded = Vec::with_capacity(types.len());
        for ty in types {
            self.read("hew_de_seq_next", self.values.ctx.i32_type().into(), &[])?;
            decoded.push((self.child(ty)?, ty));
        }
        self.read("hew_de_seq_next", self.values.ctx.i32_type().into(), &[])?;
        self.check_cursor()?;
        Ok(decoded)
    }
    /// Decode a record's members, which the source hands out in table order.
    fn record<'t>(
        &mut self,
        table: BasicValueEnum<'ctx>,
        types: &'t [ResolvedTy],
    ) -> CodegenResult<Vec<(DecodeTemporary<'ctx>, &'t ResolvedTy)>> {
        self.void("hew_de_record_begin", &[table])?;
        let mut decoded = Vec::with_capacity(types.len());
        for ty in types {
            self.read("hew_de_record_next", self.values.ctx.i32_type().into(), &[])?;
            self.check_cursor()?;
            decoded.push((self.child(ty)?, ty));
        }
        self.read("hew_de_record_next", self.values.ctx.i32_type().into(), &[])?;
        self.check_cursor()?;
        Ok(decoded)
    }
    fn aggregate(
        &self,
        plan: &SemWirePlan,
        decoded: Vec<(DecodeTemporary<'ctx>, &ResolvedTy)>,
        output: DecodeTemporary<'ctx>,
    ) -> CodegenResult<()> {
        let ctx = self.values.ctx;
        let builder = self.values.builder;
        let layout = self
            .values
            .module
            .target
            .layout(&plan.ty)
            .ok_or_else(|| CodegenError::FailClosed("codec aggregate has no layout".into()))?;
        let mut record = llvm_type(ctx, &layout.repr)?
            .into_struct_type()
            .const_zero();
        for (position, (value, ty)) in decoded.iter().enumerate() {
            record = builder
                .build_insert_value(
                    record,
                    self.load(value.slot, ty)?,
                    index(position)?,
                    "wire.record.field",
                )
                .llvm_ctx("assemble complete codec aggregate")?
                .into_struct_value();
        }
        builder
            .build_store(output.slot, record)
            .llvm_ctx("stage complete codec aggregate")?;
        self.mark(output, true)?;
        for (value, _) in decoded {
            self.mark(value, false)?;
        }
        Ok(())
    }
    #[expect(
        clippy::too_many_lines,
        reason = "one exhaustive match keeps every planned shape's decoding together"
    )]
    fn decode(&mut self, plan: &SemWirePlan, output: DecodeTemporary<'ctx>) -> CodegenResult<()> {
        let ctx = self.values.ctx;
        let builder = self.values.builder;
        match &plan.kind {
            SemWireKind::Scalar => {
                let layout =
                    self.values.module.target.layout(&plan.ty).ok_or_else(|| {
                        CodegenError::FailClosed("codec scalar has no layout".into())
                    })?;
                let ty = llvm_type(ctx, &layout.repr)?;
                let value = match &plan.ty {
                    ResolvedTy::Bool => self.read("hew_de_bool", ctx.i8_type().into(), &[])?,
                    ResolvedTy::String => self.read(
                        "hew_de_str",
                        ctx.ptr_type(AddressSpace::default()).into(),
                        &[],
                    )?,
                    ResolvedTy::Bytes => {
                        self.void("hew_de_bytes", &[output.slot.into()])?;
                        return self.mark(output, true);
                    }
                    ResolvedTy::F32 | ResolvedTy::F64 => {
                        let value = self
                            .read("hew_de_f64", ctx.f64_type().into(), &[])?
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
                    other => {
                        let value = if *other == ResolvedTy::Char {
                            self.read("hew_de_char", ctx.i32_type().into(), &[])?
                        } else {
                            self.read(
                                "hew_de_int",
                                ctx.i64_type().into(),
                                &[
                                    self.i32(u64::from(ty.into_int_type().get_bit_width())),
                                    ctx.i8_type()
                                        .const_int(u64::from(is_signed(other)), false)
                                        .into(),
                                ],
                            )?
                        };
                        builder
                            .build_int_cast(
                                value.into_int_value(),
                                ty.into_int_type(),
                                "wire.integer.narrow",
                            )
                            .llvm_ctx("store range-checked codec integer")?
                            .into()
                    }
                };
                builder
                    .build_store(output.slot, value)
                    .llvm_ctx("stage decoded scalar")?;
                self.mark(output, true)
            }
            SemWireKind::Unit => {
                self.void("hew_de_unit", &[])?;
                self.mark(output, true)
            }
            SemWireKind::Tuple(elements) => {
                let decoded = self.sequence(elements)?;
                self.aggregate(plan, decoded, output)
            }
            SemWireKind::Array { element, len } => {
                let elements = vec![
                    element.clone();
                    usize::try_from(*len).map_err(|_| {
                        CodegenError::FailClosed("codec array length exceeds usize".into())
                    })?
                ];
                let decoded = self.sequence(&elements)?;
                let layout =
                    self.values.module.target.layout(&plan.ty).ok_or_else(|| {
                        CodegenError::FailClosed("codec array has no layout".into())
                    })?;
                let mut array = llvm_type(ctx, &layout.repr)?.into_array_type().const_zero();
                for (position, (value, ty)) in decoded.iter().enumerate() {
                    array = builder
                        .build_insert_value(
                            array,
                            self.load(value.slot, ty)?,
                            index(position)?,
                            "wire.array.element",
                        )
                        .llvm_ctx("assemble complete codec array")?
                        .into_array_value();
                }
                builder
                    .build_store(output.slot, array)
                    .llvm_ctx("stage complete codec array")?;
                self.mark(output, true)?;
                for (value, _) in decoded {
                    self.mark(value, false)?;
                }
                Ok(())
            }
            SemWireKind::Record { table, fields, .. } => {
                let decoded = match table {
                    Some(members) => {
                        let table = self.table(&plan.ty, None, members);
                        self.record(table, fields)?
                    }
                    None => self.sequence(fields)?,
                };
                self.aggregate(plan, decoded, output)
            }
            SemWireKind::Option {
                none, some, value, ..
            } => {
                let null = self
                    .read("hew_de_is_null", ctx.i32_type().into(), &[])?
                    .into_int_value();
                let absent = builder
                    .build_int_compare(
                        IntPredicate::NE,
                        null,
                        ctx.i32_type().const_zero(),
                        "wire.null",
                    )
                    .llvm_ctx("test codec Option null")?;
                let empty = ctx.append_basic_block(self.values.value, "wire.none");
                let present = ctx.append_basic_block(self.values.value, "wire.some");
                let done = ctx.append_basic_block(self.values.value, "wire.option.done");
                builder
                    .build_conditional_branch(absent, empty, present)
                    .llvm_ctx("decode Option presence")?;
                builder.position_at_end(empty);
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
            SemWireKind::Enum {
                table, variants, ..
            } => {
                let enum_table = self.table(&plan.ty, None, table);
                let selected = self
                    .read("hew_de_variant", ctx.i32_type().into(), &[enum_table])?
                    .into_int_value();
                self.check_cursor()?;
                let invalid = ctx.append_basic_block(self.values.value, "wire.unknown.variant");
                let done = ctx.append_basic_block(self.values.value, "wire.enum.done");
                let cases = (0..variants.len())
                    .map(|position| {
                        Ok((
                            ctx.i32_type().const_int(u64::from(index(position)?), false),
                            ctx.append_basic_block(self.values.value, "wire.variant"),
                        ))
                    })
                    .collect::<CodegenResult<Vec<_>>>()?;
                builder
                    .build_switch(selected, invalid, &cases)
                    .llvm_ctx("select decoded codec variant")?;
                builder.position_at_end(invalid);
                self.fail_now()?;
                for (position, (payload, (_, block))) in variants.iter().zip(cases).enumerate() {
                    builder.position_at_end(block);
                    let fields = match payload {
                        SemWirePayload::Unit => Vec::new(),
                        SemWirePayload::Single(ty) => vec![(self.child(ty)?, ty)],
                        SemWirePayload::Tuple(types) => self.sequence(types)?,
                        SemWirePayload::Record { table, fields } => {
                            let table = self.table(&plan.ty, Some(position), table);
                            self.record(table, fields)?
                        }
                    };
                    self.void("hew_de_variant_end", &[])?;
                    self.check_cursor()?;
                    self.variant(plan, index(position)?, &fields, output)?;
                    builder
                        .build_unconditional_branch(done)
                        .llvm_ctx("finish codec variant decode")?;
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
                CodegenError::FailClosed(format!("codec collection descriptor `{name}` is absent"))
            })
    }
    fn probe_insert(
        &self,
        key: &ResolvedTy,
        collection: BasicValueEnum<'ctx>,
        input: PointerValue<'ctx>,
        value: Option<PointerValue<'ctx>>,
    ) -> CodegenResult<(IntValue<'ctx>, PointerValue<'ctx>)> {
        let callbacks = CollectionCallbacks {
            values: &self.values,
            frame: self.frame.as_ref(),
            callbacks: self.callbacks,
            fault: self.fault,
            status: self.status,
            failure: self.fail,
            allocations: Some(self.allocations),
        };
        let outputs = value.into_iter().map(Into::into).collect::<Vec<_>>();
        let (unique, cursor) = callbacks.probe(CollectionProbe {
            key,
            begin: if value.is_some() {
                "hew_hashmap_probe_begin"
            } else {
                "hew_hashset_probe_begin"
            },
            receiver: collection,
            input,
            inserting: true,
            commit: if value.is_some() {
                "hew_hashmap_probe_insert_take"
            } else {
                "hew_hashset_probe_insert_clone"
            },
            commit_args: &outputs,
            releases: true,
        })?;
        let unique = self
            .values
            .builder
            .build_int_compare(
                IntPredicate::NE,
                unique,
                unique.get_type().const_zero(),
                "wire.key.unique",
            )
            .llvm_ctx("check decoded key uniqueness")?;
        if let Some(frame) = &self.frame {
            frame.carry(
                self.values.ctx,
                self.values.builder,
                unique,
                "wire.key.unique.slot",
            )?;
        }
        Ok((
            unique,
            cursor.expect("codec insertion detaches displaced owners"),
        ))
    }
    /// A key the collection already holds by its own `Eq` is a duplicate.
    fn refuse_duplicate(&self, unique: IntValue<'ctx>) -> CodegenResult<()> {
        let accepted = self
            .values
            .ctx
            .append_basic_block(self.values.value, "wire.key.accepted");
        let duplicate = self
            .values
            .ctx
            .append_basic_block(self.values.value, "wire.key.duplicate");
        self.values
            .builder
            .build_conditional_branch(unique, accepted, duplicate)
            .llvm_ctx("reject repeated semantic key")?;
        self.values.builder.position_at_end(duplicate);
        self.void("hew_de_duplicate", &[])?;
        self.fail_now()?;
        self.values.builder.position_at_end(accepted);
        Ok(())
    }
    #[expect(
        clippy::too_many_lines,
        reason = "building, filling and closing one decoded collection share its cleanup state"
    )]
    fn collection(
        &mut self,
        plan: &SemWirePlan,
        key: &ResolvedTy,
        value: Option<&ResolvedTy>,
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
                    CodegenError::FailClosed("codec vector has no physical glue".into())
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
                .ok_or_else(|| CodegenError::FailClosed("codec map has no physical glue".into()))?;
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
                .ok_or_else(|| CodegenError::FailClosed("codec set has no physical glue".into()))?;
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
        if map {
            let string_keys = u64::from(*key == ResolvedTy::String);
            self.void(
                "hew_de_map_begin",
                &[ctx.i8_type().const_int(string_keys, false).into()],
            )?;
        } else if vector {
            self.void("hew_de_seq_begin", &[])?;
        } else {
            self.void("hew_de_set_begin", &[])?;
        }
        self.check_cursor()?;
        let key_slot = self.temporary(key)?;
        let value_slot = value.map(|ty| self.temporary(ty)).transpose()?;
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
                    "hew_de_map_next"
                } else {
                    "hew_de_seq_next"
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
        let load_collection = || {
            builder
                .build_load(pointer, output.slot, "wire.collection.owner")
                .llvm_ctx("restore decoded collection after callback")
        };
        if let (Some(value), Some(value_slot)) = (value, value_slot) {
            self.child_into(value, value_slot)?;
            let (unique, cursor) = self.probe_insert(
                key,
                load_collection()?,
                key_slot.slot,
                Some(value_slot.slot),
            )?;
            self.mark(value_slot, false)?;
            self.release(key_slot)?;
            release::drain(&self.values, self.frame.as_ref(), cursor)?;
            let status = builder
                .build_load(ctx.i32_type(), self.status, "wire.cleanup.status")
                .llvm_ctx("read decoded owner cleanup status")?
                .into_int_value();
            self.check_status(status)?;
            self.refuse_duplicate(unique)?;
        } else if vector {
            runtime(
                &self.values,
                "hew_vec_push_owned_move",
                None,
                &[load_collection()?, key_slot.slot.into()],
            )?;
            self.mark(key_slot, false)?;
        } else {
            let (unique, cursor) =
                self.probe_insert(key, load_collection()?, key_slot.slot, None)?;
            self.release(key_slot)?;
            release::drain(&self.values, self.frame.as_ref(), cursor)?;
            let status = builder
                .build_load(ctx.i32_type(), self.status, "wire.cleanup.status")
                .llvm_ctx("read decoded owner cleanup status")?
                .into_int_value();
            self.check_status(status)?;
            self.refuse_duplicate(unique)?;
        }
        builder
            .build_unconditional_branch(header)
            .llvm_ctx("continue decoded collection")?;
        builder.position_at_end(done);
        self.check_cursor()
    }
}

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    #[expect(
        clippy::too_many_arguments,
        reason = "the codec terminator supplies its plans, ownership recipes and exact result/fault edges"
    )]
    #[expect(
        clippy::too_many_lines,
        reason = "one entry keeps the encode result and every decode failure edge together"
    )]
    pub(super) fn emit_wire_codec(
        &self,
        codec: Codec,
        plans: &SemWirePlans,
        recipes: &BTreeMap<ResolvedTy, PhysicalValueRecipe>,
        decode_result: Option<&hew_mir::physical::PhysicalWireDecodeResult>,
        input: ArgumentTransfer,
        result: StorageId,
        normal: &PhysicalEdge,
        unwind: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let ArgumentTransfer::Borrow(input) = input else {
            return Err(CodegenError::FailClosed(
                "codec input must remain borrowed".into(),
            ));
        };
        let values = self.value_emitter();
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let callback = emit_callback(
            self.module,
            self.ctx,
            self.llvm,
            plans,
            &plans.root,
            recipes,
            self.value_callbacks,
            !codec.is_serialize(),
        )?;
        let format = self
            .ctx
            .i32_type()
            .const_int(codec.format.code(), false)
            .into();
        if codec.is_serialize() {
            let sink = runtime_value(&values, "hew_ser_new", pointer.into(), &[format])?;
            self.builder
                .build_call(
                    callback,
                    &[sink.into(), self.slots[input.0 as usize].into()],
                    "",
                )
                .llvm_ctx("encode borrowed codec value")?;
            if !codec.format.is_text() {
                runtime(
                    &values,
                    "hew_ser_finish_bytes",
                    None,
                    &[sink, self.slots[result.0 as usize].into()],
                )?;
            } else {
                let text =
                    runtime_value(&values, "hew_ser_finish_string", pointer.into(), &[sink])?;
                self.store(result, text)?;
            }
            return self.emit_result_edge(Some(result), normal);
        }
        let layout =
            self.module.target.layout(&plans.root).ok_or_else(|| {
                CodegenError::FailClosed("codec decode output lacks layout".into())
            })?;
        let decoded =
            values.entry_scratch(llvm_type(self.ctx, &layout.repr)?, "wire.decoded.value")?;
        let fault_out = values.entry_scratch(pointer.into(), "wire.callback.fault")?;
        self.builder
            .build_store(fault_out, pointer.const_null())
            .llvm_ctx("initialize codec callback fault owner")?;
        let reader = runtime_value(
            &values,
            "hew_de_new",
            pointer.into(),
            &[format, self.slots[input.0 as usize].into()],
        )?;
        if let Some(frame) = &self.frame {
            frame.carry(self.ctx, &self.builder, reader, "wire.reader.slot")?;
        }
        let arguments = [reader.into(), decoded.into(), fault_out.into()];
        let status = if decode_is_resumable(self.module, plans, &plans.root, recipes) {
            suspend::invoke_child(
                self.ctx,
                self.llvm,
                &self.builder,
                self.value,
                self.frame.as_ref().ok_or_else(|| {
                    CodegenError::FailClosed("suspending codec decode lacks a caller frame".into())
                })?,
                callback,
                &arguments,
            )?
        } else {
            self.runtime_call_value(callback, &arguments, "wire.decode.status")?
                .into_int_value()
        };
        let success = self.ctx.append_basic_block(self.value, "wire.success");
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
            .llvm_ctx("test complete codec decode")?;
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
            .llvm_ctx("preserve codec collection fault identity")?;
        self.builder.position_at_end(propagate);
        runtime(&values, "hew_de_free", None, &[reader])?;
        self.store_active_fault_value(fault, status)?;
        self.emit_edge(unwind)?;
        self.builder.position_at_end(malformed);
        let cases = decode_result.ok_or_else(|| {
            CodegenError::FailClosed("codec decode lacks its Result cases".into())
        })?;
        // The reader's error replays as a `wire.DecodeError` value, decoded
        // through that type's own plan into the `Err` case.
        let error_reader =
            runtime_value(&values, "hew_de_error_reader", pointer.into(), &[reader])?;
        runtime(&values, "hew_de_free", None, &[reader])?;
        let error_callback = emit_callback(
            self.module,
            self.ctx,
            self.llvm,
            plans,
            &cases.error_ty,
            recipes,
            self.value_callbacks,
            true,
        )?;
        let error_layout =
            self.module.target.layout(&cases.error_ty).ok_or_else(|| {
                CodegenError::FailClosed("codec decode error lacks layout".into())
            })?;
        let error_ty = llvm_type(self.ctx, &error_layout.repr)?;
        let error_slot = values.entry_scratch(error_ty, "wire.decode.error")?;
        self.runtime_call_value(
            error_callback,
            &[error_reader.into(), error_slot.into(), fault_out.into()],
            "wire.decode.error.status",
        )?;
        runtime(&values, "hew_de_free", None, &[error_reader])?;
        let error = self
            .builder
            .build_load(error_ty, error_slot, "wire.decode.error.value")
            .llvm_ctx("take decoded error owner")?;
        self.write_variant_value(
            self.slots[result.0 as usize],
            cases.error,
            &[error],
            cases.glue,
        )?;
        self.emit_result_edge(Some(result), normal)?;
        self.builder.position_at_end(success);
        runtime(&values, "hew_de_free", None, &[reader])?;
        let value = self
            .builder
            .build_load(
                llvm_type(self.ctx, &layout.repr)?,
                decoded,
                "wire.complete.value",
            )
            .llvm_ctx("take decoded value owner")?;
        self.write_variant_value(
            self.slots[result.0 as usize],
            cases.ok,
            &[value],
            cases.glue,
        )?;
        self.emit_result_edge(Some(result), normal)
    }
}
