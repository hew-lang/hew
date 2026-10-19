//! Same-build C wrappers over verified native call and value contracts.

use super::*;

#[derive(Debug)]
struct ExportedFunction {
    callable: CallableId,
    symbol: String,
}

/// Explicit checked exports and their opaque C presentation types.
#[derive(Debug)]
pub struct HostExport<'a> {
    pub(super) verified: &'a VerifiedPhysicalModule,
    functions: Vec<ExportedFunction>,
    types: Vec<(ResolvedTy, String)>,
}

impl<'a> HostExport<'a> {
    /// Check selected functions against their concrete physical signatures.
    /// # Errors
    /// Refuses process entries, missing bodies and reserved or colliding names.
    pub fn new(
        verified: &'a VerifiedPhysicalModule,
        selected: &[(CallableId, &str)],
    ) -> CodegenResult<Self> {
        let module = verified.module();
        let fail = |message: &str| CodegenError::FailClosed(message.into());
        if selected.is_empty()
            || module.entry_callable.is_some()
            || !module.test_entries.is_empty()
            || module.target.triple.starts_with("wasm")
        {
            return Err(fail(
                "C host exports require a native library without a process entry",
            ));
        }
        let mut functions = Vec::<ExportedFunction>::new();
        let mut types = Vec::<(ResolvedTy, String)>::new();
        for &(id, symbol) in selected {
            if symbol.is_empty()
                || !symbol.as_bytes()[0].is_ascii_alphabetic()
                || !symbol
                    .bytes()
                    .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_')
                || symbol.starts_with("hew_")
                || symbol.starts_with("HEW_")
                || ["HewText", "HewJson", "HewError", "HewRuntime"].contains(&symbol)
                || symbol == "main"
                || C_KEYWORDS
                    .split_ascii_whitespace()
                    .any(|word| word == symbol)
            {
                return Err(fail("C export requires a non-reserved ASCII C identifier"));
            }
            if functions.iter().any(|export| export.symbol == symbol)
                || module
                    .callables
                    .iter()
                    .any(|callee| callee.symbol == symbol)
            {
                return Err(fail("C export symbol collides with another callable"));
            }
            let body = callable(module, id)?;
            if !module
                .functions
                .iter()
                .any(|function| function.callable == id)
                || body.return_ty == ResolvedTy::Never
            {
                return Err(fail("C export requires a returning concrete function body"));
            }
            for ty in body
                .params
                .iter()
                .map(|param| &param.ty)
                .chain([&body.return_ty])
            {
                if opaque(ty) && !types.iter().any(|(known, _)| known == ty) {
                    let spelling: String = hew_types::mangle::mangle_resolved_ty(ty)
                        .bytes()
                        .map(|byte| {
                            if byte.is_ascii_alphanumeric() {
                                char::from(byte).to_string()
                            } else {
                                format!("_{byte:02x}")
                            }
                        })
                        .collect();
                    let name = format!("HewOwned_{spelling}");
                    if types.iter().any(|(_, known)| known == &name) {
                        return Err(fail("C opaque type spellings collide"));
                    }
                    types.push((ty.clone(), name));
                }
            }
            functions.push(ExportedFunction {
                callable: id,
                symbol: symbol.into(),
            });
        }
        for (_, name) in &types {
            if functions
                .iter()
                .any(|export| export.symbol == *name || export.symbol == format!("{name}_release"))
            {
                return Err(fail(
                    "C export collides with an opaque type or release function",
                ));
            }
        }
        Ok(Self {
            verified,
            functions,
            types,
        })
    }

    fn type_name(&self, ty: &ResolvedTy) -> &str {
        if let Some(name) = scalar(ty) {
            return name;
        }
        if *ty == ResolvedTy::String {
            return "HewText";
        }
        &self
            .types
            .iter()
            .find(|(known, _)| known == ty)
            .expect("checked host type")
            .1
    }

    /// C11/C++17 declarations with explicit consuming slots and borrowed owners.
    #[must_use]
    pub fn header(&self) -> String {
        let mut header = String::from("/* Experimental same-build Hew exports. */\n#include \"hew_host.h\"\n#ifdef __cplusplus\nextern \"C\" {\n#endif\n");
        for (_, name) in &self.types {
            header.push_str(&format!("typedef struct {name} {name};\nint32_t {name}_release({name} **owner, HewError **error);\n"));
        }
        for export in &self.functions {
            let body =
                callable(self.verified.module(), export.callable).expect("checked host callable");
            let mut params = body
                .params
                .iter()
                .enumerate()
                .filter(|(_, param)| param.ty != ResolvedTy::Unit)
                .map(|(index, param)| {
                    let name = self.type_name(&param.ty);
                    if opaque(&param.ty) {
                        match param.passing {
                            hew_mir::physical::SemParamPassing::Consume => {
                                format!("{name} **arg{index}")
                            }
                            hew_mir::physical::SemParamPassing::BorrowMut => {
                                format!("{name} *arg{index}")
                            }
                            _ => format!("const {name} *arg{index}"),
                        }
                    } else if param.ty == ResolvedTy::String
                        && param.carrier == ParamCarrier::Indirect
                    {
                        format!("HewText **arg{index}")
                    } else if param.ty == ResolvedTy::String {
                        format!("const HewText *arg{index}")
                    } else if param.carrier == ParamCarrier::Indirect {
                        format!("{name} *arg{index}")
                    } else {
                        format!("{name} arg{index}")
                    }
                })
                .collect::<Vec<_>>();
            if body.return_ty != ResolvedTy::Unit {
                params.push(format!(
                    "{} {}out",
                    self.type_name(&body.return_ty),
                    if scalar(&body.return_ty).is_some() {
                        "*"
                    } else {
                        "**"
                    }
                ));
            }
            params.push("HewError **error".into());
            header.push_str(&format!(
                "int32_t {}({});\n",
                export.symbol,
                params.join(", ")
            ));
        }
        header.push_str("#ifdef __cplusplus\n}\n#endif\n");
        header
    }
}

const C_KEYWORDS: &str = "alignas alignof and and_eq asm atomic_cancel atomic_commit atomic_noexcept auto bitand bitor bool break case catch char char8_t char16_t char32_t class compl concept const consteval constexpr constinit const_cast continue co_await co_return co_yield decltype default delete do double dynamic_cast else enum explicit export extern false float for friend goto if inline int long mutable namespace new noexcept not not_eq nullptr operator or or_eq private protected public register reinterpret_cast requires restrict return short signed sizeof static static_assert static_cast struct switch template this thread_local throw true try typedef typeid typename typeof typeof_unqual union unsigned using virtual void volatile wchar_t while xor xor_eq";

fn scalar(ty: &ResolvedTy) -> Option<&'static str> {
    Some(match ty {
        ResolvedTy::Bool | ResolvedTy::U8 => "uint8_t",
        ResolvedTy::I8 => "int8_t",
        ResolvedTy::I16 => "int16_t",
        ResolvedTy::U16 => "uint16_t",
        ResolvedTy::I32 => "int32_t",
        ResolvedTy::Char | ResolvedTy::U32 => "uint32_t",
        ResolvedTy::I64 => "int64_t",
        ResolvedTy::U64 => "uint64_t",
        ResolvedTy::Isize => "intptr_t",
        ResolvedTy::Usize => "uintptr_t",
        ResolvedTy::F32 => "float",
        ResolvedTy::F64 => "double",
        _ => return None,
    })
}

fn opaque(ty: &ResolvedTy) -> bool {
    scalar(ty).is_none()
        && !matches!(
            ty,
            ResolvedTy::Unit | ResolvedTy::String | ResolvedTy::Never
        )
}

pub(super) fn emit<'ctx>(
    emitter: &ModuleEmitter<'ctx, '_>,
    export: &HostExport<'_>,
) -> CodegenResult<()> {
    for body in &emitter.module.functions {
        emitter
            .functions
            .get(&body.callable)
            .ok_or_else(|| CodegenError::FailClosed("host body has no LLVM declaration".into()))?
            .set_linkage(Linkage::Internal);
    }
    for (_, name) in &export.types {
        emit_release(emitter, name)?;
    }
    for function in &export.functions {
        emit_function(emitter, function)?;
    }
    Ok(())
}

fn emit_release<'ctx>(emitter: &ModuleEmitter<'ctx, '_>, name: &str) -> CodegenResult<()> {
    let ctx = emitter.ctx;
    let ptr = ctx.ptr_type(AddressSpace::default());
    let function = emitter.llvm.add_function(
        &format!("{name}_release"),
        ctx.i32_type().fn_type(&[ptr.into(); 2], false),
        Some(Linkage::External),
    );
    let builder = ctx.create_builder();
    builder.position_at_end(ctx.append_basic_block(function, "release"));
    let slot = function.get_nth_param(0).unwrap().into_pointer_value();
    let owner = builder
        .build_load(ptr, slot, "owner")
        .llvm_ctx("load host release owner")?;
    builder
        .build_store(slot, ptr.const_null())
        .llvm_ctx("consume host release owner")?;
    let release = get_or_declare_external(
        &emitter.llvm,
        "hew_host_value_release",
        ctx.i32_type().fn_type(&[ptr.into(); 2], false),
    )?;
    let status = suspend::call_value(
        &builder,
        release,
        &[owner.into(), function.get_nth_param(1).unwrap().into()],
        "release.status",
    )?;
    builder
        .build_return(Some(&status))
        .llvm_ctx("return host release status")?;
    Ok(())
}

fn input_arguments<'ctx>(
    emitter: &ModuleEmitter<'ctx, '_>,
    builder: &Builder<'ctx>,
    wrapper: FunctionValue<'ctx>,
    body: &PhysicalCallable,
) -> CodegenResult<(Vec<BasicMetadataValueEnum<'ctx>>, Vec<PointerValue<'ctx>>)> {
    let ctx = emitter.ctx;
    let ptr = ctx.ptr_type(AddressSpace::default());
    let mut arguments = Vec::new();
    let mut consumed = Vec::new();
    let mut index = 0;
    for param in &body.params {
        if param.ty == ResolvedTy::Unit {
            let ty = llvm_type(ctx, &param.layout.repr)?;
            arguments.push(if param.carrier == ParamCarrier::Indirect {
                builder
                    .build_alloca(ty, "unit.argument")
                    .llvm_ctx("realize host unit argument")?
                    .into()
            } else {
                ty.const_zero().into()
            });
            continue;
        }
        let input = wrapper.get_nth_param(index as u32).unwrap();
        index += 1;
        let value = if opaque(&param.ty) {
            let mut owner = input.into_pointer_value();
            if param.passing == hew_mir::physical::SemParamPassing::Consume {
                let slot = owner;
                owner = builder
                    .build_load(ptr, slot, "consumed.owner")
                    .llvm_ctx("load consumed host owner")?
                    .into_pointer_value();
                builder
                    .build_store(slot, ptr.const_null())
                    .llvm_ctx("handoff consumed host owner")?;
                consumed.push(owner);
            }
            let storage = call_pointer(emitter, builder, "hew_host_value_storage", owner)?;
            if param.carrier == ParamCarrier::Indirect {
                storage.into()
            } else {
                builder
                    .build_load(llvm_type(ctx, &param.layout.repr)?, storage, "argument")
                    .llvm_ctx("load direct host value")?
            }
        } else if param.ty == ResolvedTy::String
            && param.passing == hew_mir::physical::SemParamPassing::Consume
        {
            call_pointer(
                emitter,
                builder,
                "hew_string_clone",
                input.into_pointer_value(),
            )?
            .into()
        } else {
            input
        };
        arguments.push(value.into());
    }
    Ok((arguments, consumed))
}

fn finish_arguments<'ctx>(
    emitter: &ModuleEmitter<'ctx, '_>,
    builder: &Builder<'ctx>,
    wrapper: FunctionValue<'ctx>,
    body: &PhysicalCallable,
    consumed: &[PointerValue<'ctx>],
) -> CodegenResult<()> {
    let ptr = emitter.ctx.ptr_type(AddressSpace::default());
    for (index, param) in body
        .params
        .iter()
        .filter(|param| param.ty != ResolvedTy::Unit)
        .enumerate()
    {
        if param.ty == ResolvedTy::String && param.carrier == ParamCarrier::Indirect {
            let slot = wrapper
                .get_nth_param(index as u32)
                .unwrap()
                .into_pointer_value();
            let text = builder
                .build_load(ptr, slot, "updated.text")
                .llvm_ctx("load updated host text")?
                .into_pointer_value();
            let text = call_pointer(emitter, builder, "hew_host_text_from_owned", text)?;
            builder
                .build_store(slot, text)
                .llvm_ctx("normalize updated host text")?;
        }
    }
    for &owner in consumed {
        call_forget(emitter, builder, owner)?;
    }
    Ok(())
}

fn emit_function<'ctx>(
    emitter: &ModuleEmitter<'ctx, '_>,
    export: &ExportedFunction,
) -> CodegenResult<()> {
    let body = callable(emitter.module, export.callable)?;
    let ctx = emitter.ctx;
    let ptr = ctx.ptr_type(AddressSpace::default());
    let mut params = body
        .params
        .iter()
        .filter(|param| param.ty != ResolvedTy::Unit)
        .map(|param| {
            if opaque(&param.ty)
                || param.ty == ResolvedTy::String
                || param.carrier == ParamCarrier::Indirect
            {
                Ok(ptr.into())
            } else {
                Ok(llvm_type(ctx, &param.layout.repr)?.into())
            }
        })
        .collect::<CodegenResult<Vec<BasicMetadataTypeEnum<'ctx>>>>()?;
    if body.return_ty != ResolvedTy::Unit {
        params.push(ptr.into());
    }
    params.push(ptr.into());
    let wrapper = emitter.llvm.add_function(
        &export.symbol,
        ctx.i32_type().fn_type(&params, false),
        Some(Linkage::External),
    );
    let builder = ctx.create_builder();
    builder.position_at_end(ctx.append_basic_block(wrapper, "entry"));
    let success = ctx.append_basic_block(wrapper, "success");
    let failure = ctx.append_basic_block(wrapper, "failure");
    let (mut arguments, consumed) = input_arguments(emitter, &builder, wrapper, body)?;
    let public_param_count = body
        .params
        .iter()
        .filter(|param| param.ty != ResolvedTy::Unit)
        .count();
    let out = (body.return_ty != ResolvedTy::Unit).then(|| {
        wrapper
            .get_nth_param(public_param_count as u32)
            .unwrap()
            .into_pointer_value()
    });
    let error = wrapper.get_last_param().unwrap().into_pointer_value();
    builder
        .build_store(error, ptr.const_null())
        .llvm_ctx("initialize host error")?;
    let (result, result_owner) = result_storage(emitter, &builder, body, &export.symbol)?;
    if let Some(out) = out {
        if scalar(&body.return_ty).is_none() {
            builder
                .build_store(out, ptr.const_null())
                .llvm_ctx("initialize host output")?;
        }
        arguments.push(result.into());
    }
    let fault = builder
        .build_alloca(ptr, "fault")
        .llvm_ctx("allocate host fault")?;
    builder
        .build_store(fault, ptr.const_null())
        .llvm_ctx("initialize host fault")?;
    arguments.push(fault.into());
    let status = suspend::call_value(&builder, emitter.functions[&body.id], &arguments, "status")?
        .into_int_value();
    finish_arguments(emitter, &builder, wrapper, body, &consumed)?;
    let ok = builder
        .build_int_compare(IntPredicate::EQ, status, ctx.i32_type().const_zero(), "ok")
        .llvm_ctx("check host status")?;
    builder
        .build_conditional_branch(ok, success, failure)
        .llvm_ctx("branch host status")?;
    builder.position_at_end(success);
    if let Some(out) = out {
        publish_result(emitter, &builder, body, out, result, result_owner)?;
    }
    builder
        .build_return(Some(&ctx.i32_type().const_zero()))
        .llvm_ctx("return host success")?;
    builder.position_at_end(failure);
    if let Some(owner) = result_owner {
        call_forget(emitter, &builder, owner)?;
    }
    let fault = builder
        .build_load(ptr, fault, "failure.fault")
        .llvm_ctx("load host failure")?
        .into_pointer_value();
    let failure = call_pointer(emitter, &builder, "hew_fault_into_host_error", fault)?;
    builder
        .build_store(error, failure)
        .llvm_ctx("publish host failure")?;
    let code = get_or_declare_external(
        &emitter.llvm,
        "hew_host_error_code",
        ctx.i32_type().fn_type(&[ptr.into()], false),
    )?;
    let code = suspend::call_value(&builder, code, &[failure.into()], "error.code")?;
    builder
        .build_return(Some(&code))
        .llvm_ctx("return host failure")?;
    Ok(())
}

fn result_storage<'ctx>(
    emitter: &ModuleEmitter<'ctx, '_>,
    builder: &Builder<'ctx>,
    body: &PhysicalCallable,
    symbol: &str,
) -> CodegenResult<(PointerValue<'ctx>, Option<PointerValue<'ctx>>)> {
    if opaque(&body.return_ty) {
        let recipe = body.return_recipe.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("host result has no canonical value recipe".into())
        })?;
        let descriptor = format!("{symbol}$host_result");
        emitter.emit_value_descriptor(&descriptor, recipe)?;
        let owner = call_pointer(
            emitter,
            builder,
            "hew_host_value_new",
            emitter
                .llvm
                .get_global(&descriptor)
                .unwrap()
                .as_pointer_value(),
        )?;
        Ok((
            call_pointer(emitter, builder, "hew_host_value_storage", owner)?,
            Some(owner),
        ))
    } else if let Some(layout) = &body.return_layout {
        Ok((
            builder
                .build_alloca(llvm_type(emitter.ctx, &layout.repr)?, "result")
                .llvm_ctx("allocate host result")?,
            None,
        ))
    } else {
        Ok((
            emitter.ctx.ptr_type(AddressSpace::default()).const_null(),
            None,
        ))
    }
}

fn publish_result<'ctx>(
    emitter: &ModuleEmitter<'ctx, '_>,
    builder: &Builder<'ctx>,
    body: &PhysicalCallable,
    out: PointerValue<'ctx>,
    result: PointerValue<'ctx>,
    owner: Option<PointerValue<'ctx>>,
) -> CodegenResult<()> {
    let value: BasicValueEnum<'ctx> = if let Some(owner) = owner {
        owner.into()
    } else {
        let value = builder
            .build_load(
                llvm_type(emitter.ctx, &body.return_layout.as_ref().unwrap().repr)?,
                result,
                "value",
            )
            .llvm_ctx("load host result")?;
        if body.return_ty == ResolvedTy::String {
            call_pointer(
                emitter,
                builder,
                "hew_host_text_from_owned",
                value.into_pointer_value(),
            )?
            .into()
        } else {
            value
        }
    };
    builder
        .build_store(out, value)
        .llvm_ctx("publish host result")?;
    Ok(())
}

fn call_pointer<'ctx>(
    emitter: &ModuleEmitter<'ctx, '_>,
    builder: &Builder<'ctx>,
    symbol: &str,
    value: PointerValue<'ctx>,
) -> CodegenResult<PointerValue<'ctx>> {
    let function = external_unary_ptr(emitter.ctx, &emitter.llvm, symbol)?;
    Ok(suspend::call_value(builder, function, &[value.into()], "host.owner")?.into_pointer_value())
}

fn call_forget<'ctx>(
    emitter: &ModuleEmitter<'ctx, '_>,
    builder: &Builder<'ctx>,
    owner: PointerValue<'ctx>,
) -> CodegenResult<()> {
    let ptr = emitter.ctx.ptr_type(AddressSpace::default());
    let function = get_or_declare_external(
        &emitter.llvm,
        "hew_host_value_forget",
        emitter.ctx.void_type().fn_type(&[ptr.into()], false),
    )?;
    builder
        .build_call(function, &[owner.into()], "")
        .llvm_ctx("free consumed host storage")?;
    Ok(())
}
