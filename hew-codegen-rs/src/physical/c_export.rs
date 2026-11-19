//! Ordinary C signatures over checked Hew bodies. The private fault and result
//! slots never cross the foreign frame.
use super::*;
use hew_mir::physical::{CParameterAbi, PhysicalExternResultAbi};
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::types::AnyType;
use inkwell::values::BasicValue;

pub(super) fn emit(emitter: &ModuleEmitter<'_, '_>) -> CodegenResult<()> {
    for (declaration, symbol) in &emitter.module.c_abi.exports {
        let body = emitter
            .module
            .callables
            .iter()
            .find(|c| {
                c.declaration == *declaration
                    && matches!(c.instance, hew_mir::physical::CallableInstance::Monomorphic)
            })
            .ok_or_else(|| {
                CodegenError::FailClosed(format!("C export `{symbol}` has no selected body"))
            })?;
        emit_one(emitter, body, symbol)?;
    }
    Ok(())
}

pub(super) fn memory_attributes(
    index: u32,
    ctx: &Context,
    ty: BasicTypeEnum<'_>,
    name: &str,
    align: u32,
) -> [(u32, Attribute); 2] {
    [
        ctx.create_type_attribute(
            Attribute::get_named_enum_kind_id(name),
            ty.as_any_type_enum(),
        ),
        ctx.create_enum_attribute(Attribute::get_named_enum_kind_id("align"), u64::from(align)),
    ]
    .map(|attribute| (index, attribute))
}

fn stack_alignment(index: u32, ctx: &Context, align: u32) -> (u32, Attribute) {
    (
        index,
        ctx.create_enum_attribute(
            Attribute::get_named_enum_kind_id("alignstack"),
            u64::from(align),
        ),
    )
}

impl<'ctx> ValueEmitter<'_, 'ctx> {
    /// Reinterpret register bits via storage sized and aligned for both views.
    /// Zero-initialize tail bytes that exist only in the ABI register carrier.
    pub(super) fn c_reinterpret(
        &self,
        value: BasicValueEnum<'ctx>,
        dest: BasicTypeEnum<'ctx>,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        if value.get_type() == dest {
            return Ok(value);
        }
        if let (BasicValueEnum::IntValue(value), BasicTypeEnum::IntType(dest)) = (value, dest) {
            if value.get_type().get_bit_width() == 1 {
                return self
                    .builder
                    .build_int_z_extend(value, dest, "c.bool")
                    .map(Into::into)
                    .llvm_ctx("widen C truth");
            }
            if dest.get_bit_width() == 1 {
                return self
                    .builder
                    .build_int_compare(
                        IntPredicate::NE,
                        value,
                        value.get_type().const_zero(),
                        "c.truth",
                    )
                    .map(Into::into)
                    .llvm_ctx("project C truth");
            }
        }
        let src = value.get_type();
        let (src_size, src_align) = measure_layout(self.target_data, src);
        let (dest_size, dest_align) = measure_layout(self.target_data, dest);
        let largest = if src_size >= dest_size { src } else { dest };
        let slot = self.entry_scratch(largest, "c.carrier")?;
        slot.as_instruction()
            .expect("alloca")
            .set_alignment(src_align.max(dest_align))
            .map_err(|e| CodegenError::FailClosed(e.to_string()))?;
        if dest_size > src_size {
            self.builder
                .build_store(slot, largest.const_zero())
                .llvm_ctx("initialize C carrier padding")?;
        }
        self.builder
            .build_store(slot, value)
            .llvm_ctx("store C representation")?;
        self.builder
            .build_load(dest, slot, "c.value")
            .llvm_ctx("load converted C value")
    }
}

fn emit_one<'ctx>(
    emitter: &ModuleEmitter<'ctx, '_>,
    body: &PhysicalCallable,
    symbol: &str,
) -> CodegenResult<()> {
    let ctx = emitter.ctx;
    let ptr = ctx.ptr_type(AddressSpace::default());
    let target = &emitter.module.target;
    let result = target
        .extern_result_abi(&body.return_ty)
        .map_err(|e| CodegenError::FailClosed(e.to_string()))?;
    let plans = target
        .c_parameters(
            body.params.iter().map(|p| &p.ty),
            &emitter.module.c_abi.records,
            &result,
        )
        .map_err(|e| CodegenError::FailClosed(e.to_string()))?;
    let wrapper = declare(emitter, body, symbol, &result, &plans)?;
    let result_ty = wrapper.get_type().get_return_type();
    let builder = ctx.create_builder();
    let entry = ctx.append_basic_block(wrapper, "entry");
    builder.position_at_end(entry);
    let ve = ValueEmitter {
        module: emitter.module,
        target_data: emitter.target_data,
        ctx,
        llvm: &emitter.llvm,
        builder: &builder,
        value: wrapper,
        fault_sink: None,
    };
    let mut incoming = wrapper.get_param_iter();
    let destination = if matches!(result, PhysicalExternResultAbi::Indirect) {
        incoming.next().map(BasicValueEnum::into_pointer_value)
    } else {
        body.return_layout
            .as_ref()
            .map(|l| ve.entry_scratch(llvm_type(ctx, &l.repr)?, "c.result"))
            .transpose()?
    };
    let mut private_args: Vec<BasicMetadataValueEnum> = Vec::new();
    for (param, plan) in body.params.iter().zip(&plans) {
        private_args.push(ve.c_entry_argument(param, plan, &mut incoming)?);
    }
    if let Some(dest) = destination {
        private_args.push(dest.into());
    }
    let fault = ve.entry_scratch(ptr.into(), "c.fault")?;
    builder
        .build_store(fault, ptr.const_null())
        .llvm_ctx("initialize C entry fault")?;
    private_args.push(fault.into());
    let status = builder
        .build_call(emitter.functions[&body.id], &private_args, "c.status")
        .llvm_ctx("invoke C export body")?
        .try_as_basic_value()
        .basic()
        .expect("private Hew status")
        .into_int_value();
    ve.check_c_entry_status(status, fault)?;
    let value = if let (Some(ty), Some(layout), Some(dest)) =
        (result_ty, &body.return_layout, destination)
    {
        let value = builder
            .build_load(llvm_type(ctx, &layout.repr)?, dest, "c.result.value")
            .llvm_ctx("read Hew export result")?;
        Some(ve.c_reinterpret(value, ty)?)
    } else {
        None
    };
    builder
        .build_return(value.as_ref().map(|v| v as &dyn BasicValue))
        .llvm_ctx("return C export result")?;
    Ok(())
}

fn declare<'ctx>(
    emitter: &ModuleEmitter<'ctx, '_>,
    body: &PhysicalCallable,
    symbol: &str,
    result: &PhysicalExternResultAbi,
    plans: &[CParameterAbi],
) -> CodegenResult<FunctionValue<'ctx>> {
    let ctx = emitter.ctx;
    let ptr = ctx.ptr_type(AddressSpace::default());
    let mut args: Vec<BasicMetadataTypeEnum> = Vec::new();
    let mut attrs = Vec::new();
    let result_ty = match result {
        PhysicalExternResultAbi::Direct => body
            .return_layout
            .as_ref()
            .map(|l| llvm_type(ctx, &l.repr))
            .transpose()?,
        PhysicalExternResultAbi::Coerce(repr) => Some(llvm_type(ctx, repr)?),
        PhysicalExternResultAbi::Indirect => {
            let layout = body
                .return_layout
                .as_ref()
                .ok_or_else(|| CodegenError::FailClosed("missing C result layout".into()))?;
            args.push(ptr.into());
            attrs.extend(memory_attributes(
                0,
                ctx,
                llvm_type(ctx, &layout.repr)?,
                "sret",
                layout.align,
            ));
            None
        }
    };
    for (p, plan) in body.params.iter().zip(plans) {
        let storage = llvm_type(ctx, &p.layout.repr)?;
        match plan {
            CParameterAbi::Direct => args.push(if p.ty == ResolvedTy::Bool {
                ctx.bool_type().into()
            } else {
                storage.into()
            }),
            CParameterAbi::Coerce { repr, stack_align } => {
                if let Some(align) = stack_align {
                    attrs.push(stack_alignment(args.len() as u32, ctx, *align));
                }
                args.push(llvm_type(ctx, repr)?.into());
            }
            CParameterAbi::Expand(parts) => {
                for part in parts {
                    args.push(llvm_type(ctx, &part.repr)?.into());
                }
            }
            CParameterAbi::Indirect { byval_align } => {
                if let Some(align) = byval_align {
                    attrs.extend(memory_attributes(
                        args.len() as u32,
                        ctx,
                        storage,
                        "byval",
                        *align,
                    ));
                }
                args.push(ptr.into());
            }
        }
    }
    let signature = result_ty.map_or_else(
        || ctx.void_type().fn_type(&args, false),
        |ret| ret.fn_type(&args, false),
    );
    if emitter.llvm.get_function(symbol).is_some() || emitter.llvm.get_global(symbol).is_some() {
        return Err(CodegenError::FailClosed(format!(
            "C export symbol `{symbol}` collides with an existing symbol"
        )));
    }
    let wrapper = emitter
        .llvm
        .add_function(symbol, signature, Some(Linkage::External));
    for (idx, attr) in attrs {
        wrapper.add_attribute(AttributeLoc::Param(idx), attr);
    }
    if let Some(kind) = Widen::of(&body.return_ty).or_else(|| {
        result_ty.and_then(|ty| match ty {
            BasicTypeEnum::IntType(ty) if ty.get_bit_width() < 32 => Some(Widen::Zero),
            _ => None,
        })
    }) {
        wrapper.add_attribute(AttributeLoc::Return, kind.attribute(wrapper));
    }
    Ok(wrapper)
}

impl<'ctx> ValueEmitter<'_, 'ctx> {
    fn c_entry_argument(
        &self,
        param: &hew_mir::physical::PhysicalParam,
        plan: &CParameterAbi,
        incoming: &mut impl Iterator<Item = BasicValueEnum<'ctx>>,
    ) -> CodegenResult<BasicMetadataValueEnum<'ctx>> {
        let storage = llvm_type(self.ctx, &param.layout.repr)?;

        let value = match plan {
            CParameterAbi::Indirect { .. } => {
                let ptr = incoming
                    .next()
                    .expect("checked C parameter count")
                    .into_pointer_value();
                self.builder
                    .build_load(storage, ptr, "c.record")
                    .llvm_ctx("read C record copy")?
            }
            CParameterAbi::Expand(parts) => {
                let ty =
                    llvm_type(self.ctx, &PhysicalRepr::Struct(parts.clone()))?.into_struct_type();
                let mut aggregate = ty.get_undef();
                for i in 0..parts.len() {
                    aggregate = self
                        .builder
                        .build_insert_value(
                            aggregate,
                            incoming.next().expect("checked C parameter count"),
                            i as u32,
                            "c.part",
                        )
                        .llvm_ctx("assemble C record registers")?
                        .into_struct_value();
                }
                self.c_reinterpret(aggregate.into(), storage)?
            }
            _ => {
                self.c_reinterpret(incoming.next().expect("checked C parameter count"), storage)?
            }
        };
        Ok(match param.carrier {
            ParamCarrier::Direct => value.into(),
            ParamCarrier::Indirect => {
                let slot = self.entry_scratch(storage, "c.input")?;
                self.builder
                    .build_store(slot, value)
                    .llvm_ctx("initialize Hew record input")?;
                slot.into()
            }
        })
    }

    fn check_c_entry_status(
        &self,
        status: IntValue<'ctx>,
        fault: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let ctx = self.ctx;
        let ptr = ctx.ptr_type(AddressSpace::default());
        let ok = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                ctx.i32_type().const_zero(),
                "c.ok",
            )
            .llvm_ctx("check C export status")?;
        let success = ctx.append_basic_block(self.value, "success");
        let failure = ctx.append_basic_block(self.value, "fault");
        self.builder
            .build_conditional_branch(ok, success, failure)
            .llvm_ctx("dispatch C export status")?;
        self.builder.position_at_end(failure);
        let record = self
            .builder
            .build_load(ptr, fault, "c.raised")
            .llvm_ctx("read terminal C export fault")?;
        self.builder
            .build_call(
                external_fault_report_entry(ctx, self.llvm)?,
                &[record.into()],
                "",
            )
            .llvm_ctx("report terminal C export fault")?;
        self.builder
            .build_call(external_fault_drop(ctx, self.llvm)?, &[record.into()], "")
            .llvm_ctx("release terminal C export fault")?;
        let trap = Intrinsic::find("llvm.trap")
            .and_then(|i| i.get_declaration(self.llvm, &[]))
            .ok_or_else(|| CodegenError::FailClosed("missing LLVM trap".into()))?;
        self.builder
            .build_call(trap, &[], "")
            .llvm_ctx("terminate faulting C export")?;
        self.builder
            .build_unreachable()
            .llvm_ctx("seal faulting C export")?;
        self.builder.position_at_end(success);
        Ok(())
    }
}

type CArguments<'ctx> = (
    Vec<BasicValueEnum<'ctx>>,
    Vec<(u32, Widen)>,
    Vec<(u32, Attribute)>,
);

impl<'ctx> ValueEmitter<'_, 'ctx> {
    pub(super) fn c_arguments(
        &self,
        values: &[BasicValueEnum<'ctx>],
        widen: &[(u32, Widen)],
        types: &[&ResolvedTy],
        result: &PhysicalExternResultAbi,
    ) -> CodegenResult<CArguments<'ctx>> {
        if types.is_empty() || self.module.c_abi.records.is_empty() {
            return Ok((values.to_vec(), widen.to_vec(), Vec::new()));
        }
        if values.len() != types.len() {
            return Err(CodegenError::FailClosed(
                "C argument values and checked source types differ in length".into(),
            ));
        }
        let plans = self
            .module
            .target
            .c_parameters(types.iter().copied(), &self.module.c_abi.records, result)
            .map_err(|e| CodegenError::FailClosed(e.to_string()))?;
        let mut registers = Vec::new();
        let mut widening = Vec::new();
        let mut attrs = Vec::new();
        for (i, (value, plan)) in values.iter().copied().zip(plans).enumerate() {
            let idx = registers.len() as u32;
            match plan {
                CParameterAbi::Direct => {
                    if let Some((_, kind)) = widen.iter().find(|(n, _)| *n == i as u32) {
                        widening.push((idx, *kind));
                    }
                    registers.push(value);
                }
                CParameterAbi::Indirect { byval_align } => {
                    let storage = value.get_type();
                    let slot = self.entry_scratch(storage, "c.copy")?;
                    if let Some(align) = byval_align {
                        slot.as_instruction()
                            .expect("alloca")
                            .set_alignment(align)
                            .map_err(|e| CodegenError::FailClosed(e.to_string()))?;
                        attrs.extend(memory_attributes(idx, self.ctx, storage, "byval", align));
                    }
                    self.builder
                        .build_store(slot, value)
                        .llvm_ctx("copy C record argument")?;
                    registers.push(slot.into());
                }
                CParameterAbi::Coerce { repr, stack_align } => {
                    if let Some(align) = stack_align {
                        attrs.push(stack_alignment(idx, self.ctx, align));
                    }
                    let reg = self.c_reinterpret(value, llvm_type(self.ctx, &repr)?)?;
                    if matches!(reg.get_type(), BasicTypeEnum::IntType(int) if int.get_bit_width()<32)
                    {
                        widening.push((idx, Widen::Zero));
                    }
                    registers.push(reg);
                }
                CParameterAbi::Expand(parts) => {
                    let len = parts.len();
                    let aggregate = self
                        .c_reinterpret(value, llvm_type(self.ctx, &PhysicalRepr::Struct(parts))?)?
                        .into_struct_value();
                    for n in 0..len {
                        let reg = self
                            .builder
                            .build_extract_value(aggregate, n as u32, "c.eightbyte")
                            .llvm_ctx("extract C record register")?;
                        if matches!(reg.get_type(), BasicTypeEnum::IntType(int) if int.get_bit_width()<32)
                        {
                            widening.push((registers.len() as u32, Widen::Zero));
                        }
                        registers.push(reg);
                    }
                }
            }
        }
        Ok((registers, widening, attrs))
    }
}
