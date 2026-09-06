//! Experimental same-build C wrappers over the verified native status ABI.

use super::*;

/// A checked synchronous string-in/string-out export of one physical module.
/// The borrowed module prevents callers from changing its verified contract.
#[derive(Debug)]
pub struct HostExport<'a> {
    pub(super) verified: &'a VerifiedPhysicalModule,
    callable: CallableId,
    symbol: String,
}

impl<'a> HostExport<'a> {
    /// Validate the selected callable and the library's supported execution domain.
    ///
    /// # Errors
    /// Refuses process entries, unsupported signatures, reserved C names and
    /// operations outside synchronous scalar/string execution.
    pub fn new(
        verified: &'a VerifiedPhysicalModule,
        selected: CallableId,
        symbol: &str,
    ) -> CodegenResult<Self> {
        let module = verified.module();
        let fail = |message: &str| CodegenError::FailClosed(message.into());
        if symbol.is_empty()
            || !symbol.as_bytes()[0].is_ascii_alphabetic()
            || !symbol
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_')
            || symbol.starts_with("hew_")
            || symbol == "main"
            || C_KEYWORDS
                .split_ascii_whitespace()
                .any(|word| word == symbol)
        {
            return Err(fail("C export requires a non-reserved ASCII C identifier"));
        }
        if module.entry_callable.is_some() || module.target.triple.starts_with("wasm") {
            return Err(fail(
                "C host exports require a native library without a process entry",
            ));
        }
        if module
            .callables
            .iter()
            .any(|callee| callee.symbol == symbol)
        {
            return Err(fail("C export symbol collides with a private Hew callable"));
        }
        let callee = callable(module, selected)?;
        if !matches!(callee.params.as_slice(), [param]
            if param.ty == ResolvedTy::String
                && param.carrier == ParamCarrier::Direct
                && matches!(param.passing, hew_mir::physical::SemParamPassing::Borrow | hew_mir::physical::SemParamPassing::Consume))
            || callee.return_ty != ResolvedTy::String
            || !module
                .functions
                .iter()
                .any(|body| body.callable == selected)
        {
            return Err(fail(
                "C export requires one shared or consumed string parameter and a string result",
            ));
        }
        for body in &module.functions {
            if body.storage.iter().any(|storage| !host_value(&storage.ty)) {
                return Err(fail(
                    "C host export currently supports only scalar and string values",
                ));
            }
            for block in &body.blocks {
                match &block.terminator {
                    PhysicalTerminator::GeneratorYield { .. }
                    | PhysicalTerminator::GeneratorNext { .. }
                    | PhysicalTerminator::GeneratorClose { .. }
                    | PhysicalTerminator::IndirectCall { .. }
                    | PhysicalTerminator::Sleep { .. }
                    | PhysicalTerminator::TaskSelect { .. }
                    | PhysicalTerminator::TaskAwait { .. }
                    | PhysicalTerminator::TaskScopeJoin { .. }
                    | PhysicalTerminator::ValueCall { .. }
                    | PhysicalTerminator::SwitchVariant { .. } => {
                        return Err(fail(
                            "C host export does not yet admit indirect calls or callbacks",
                        ));
                    }
                    PhysicalTerminator::ActorCall { .. } => {
                        return Err(fail("C host exports cannot start or submit actor work"))
                    }
                    PhysicalTerminator::RuntimeCall { action, .. } => {
                        if !host_runtime(*action) {
                            return Err(fail(
                                "C host export contains an unsupported runtime operation",
                            ));
                        }
                    }
                    PhysicalTerminator::Return { .. }
                    | PhysicalTerminator::Goto(_)
                    | PhysicalTerminator::Branch { .. }
                    | PhysicalTerminator::CheckedBinary { .. }
                    | PhysicalTerminator::Call { .. }
                    | PhysicalTerminator::Panic { .. }
                    | PhysicalTerminator::EnterDefer { .. }
                    | PhysicalTerminator::FinishDefer { .. }
                    | PhysicalTerminator::CleanupDispatch { .. }
                    | PhysicalTerminator::RecoverFault { .. }
                    | PhysicalTerminator::CheckedRaiseFault { .. }
                    | PhysicalTerminator::Trap(_)
                    | PhysicalTerminator::PropagateFault
                    | PhysicalTerminator::Unreachable => {}
                }
            }
        }
        Ok(Self {
            verified,
            callable: selected,
            symbol: symbol.into(),
        })
    }

    /// Declaration matching the emitted wrapper and the shared opaque host API.
    #[must_use]
    pub fn header(&self) -> String {
        format!(
            "/* Experimental same-build synchronous Hew export. */\n#include \"hew_host.h\"\n\n#ifdef __cplusplus\nextern \"C\" {{\n#endif\n/* Borrow input; return an independent owner or an owned error.\n * All hew_host.h validity, slot and lifetime requirements apply. */\nint32_t {}(const HewText *input, HewText **out, HewError **error);\n#ifdef __cplusplus\n}}\n#endif\n",
            self.symbol
        )
    }
}

// The generated header is valid in both C11 and C++17. Include the newer
// reserved spellings too so the same declaration remains usable by newer clients.
const C_KEYWORDS: &str = "alignas alignof and and_eq asm atomic_cancel atomic_commit atomic_noexcept \
    auto bitand bitor bool break case catch char char8_t char16_t char32_t class compl concept \
    const consteval constexpr constinit const_cast continue co_await co_return co_yield decltype \
    default delete do double dynamic_cast else enum explicit export extern false float for friend \
    goto if inline int long mutable namespace new noexcept not not_eq nullptr operator or or_eq \
    private protected public register reinterpret_cast requires restrict return short signed sizeof \
    static static_assert static_cast struct switch template this thread_local throw true try typedef \
    typeid typename typeof typeof_unqual union unsigned using virtual void volatile wchar_t while xor xor_eq";

fn host_value(ty: &ResolvedTy) -> bool {
    matches!(
        ty,
        ResolvedTy::String
            | ResolvedTy::Unit
            | ResolvedTy::Bool
            | ResolvedTy::Char
            | ResolvedTy::I8
            | ResolvedTy::I16
            | ResolvedTy::I32
            | ResolvedTy::I64
            | ResolvedTy::U8
            | ResolvedTy::U16
            | ResolvedTy::U32
            | ResolvedTy::U64
            | ResolvedTy::Isize
            | ResolvedTy::Usize
            | ResolvedTy::F32
            | ResolvedTy::F64
    )
}

fn host_runtime(action: PhysicalRuntimeAction) -> bool {
    matches!(
        action,
        PhysicalRuntimeAction::StringConcat
            | PhysicalRuntimeAction::StringEquals
            | PhysicalRuntimeAction::StringStartsWith
            | PhysicalRuntimeAction::StringIsEmpty
            | PhysicalRuntimeAction::StringToUppercase
            | PhysicalRuntimeAction::StringTrim
            | PhysicalRuntimeAction::StringLen
            | PhysicalRuntimeAction::StringByteLen
            | PhysicalRuntimeAction::U8ToString
            | PhysicalRuntimeAction::I64ToString
    )
}

pub(super) fn emit<'ctx>(
    emitter: &ModuleEmitter<'ctx, '_>,
    export: &HostExport<'_>,
) -> CodegenResult<()> {
    // The explicit C wrapper roots this library. Its private implementations
    // stay local even when another generated library has the same helper names.
    for body in &emitter.module.functions {
        emitter
            .functions
            .get(&body.callable)
            .ok_or_else(|| {
                CodegenError::FailClosed("host library body has no LLVM declaration".into())
            })?
            .set_linkage(Linkage::Internal);
    }
    let ctx = emitter.ctx;
    let ptr = ctx.ptr_type(AddressSpace::default());
    let wrapper = emitter.llvm.add_function(
        &export.symbol,
        ctx.i32_type()
            .fn_type(&[ptr.into(), ptr.into(), ptr.into()], false),
        Some(Linkage::External),
    );
    let entry = ctx.append_basic_block(wrapper, "entry");
    let success = ctx.append_basic_block(wrapper, "success");
    let failure = ctx.append_basic_block(wrapper, "failure");
    let builder = ctx.create_builder();
    builder.position_at_end(entry);
    let params = wrapper.get_params();
    let mut input = params[0].into_pointer_value();
    let out = params[1].into_pointer_value();
    let error = params[2].into_pointer_value();
    builder
        .build_store(out, ptr.const_null())
        .llvm_ctx("initialize host result")?;
    builder
        .build_store(error, ptr.const_null())
        .llvm_ctx("initialize host error")?;
    let result = builder
        .build_alloca(ptr, "result")
        .llvm_ctx("allocate private result")?;
    let fault = builder
        .build_alloca(ptr, "fault")
        .llvm_ctx("allocate private fault")?;
    builder
        .build_store(fault, ptr.const_null())
        .llvm_ctx("initialize private fault")?;
    if callable(emitter.module, export.callable)?.params[0].passing
        == hew_mir::physical::SemParamPassing::Consume
    {
        input = call_pointer(emitter, &builder, "hew_string_clone", input)?;
    }
    let callee = emitter
        .functions
        .get(&export.callable)
        .ok_or_else(|| CodegenError::FailClosed("host export has no LLVM body".into()))?;
    let status = builder
        .build_call(
            *callee,
            &[input.into(), result.into(), fault.into()],
            "status",
        )
        .llvm_ctx("call host-selected Hew function")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("Hew body returned no status".into()))?
        .into_int_value();
    let ok = builder
        .build_int_compare(IntPredicate::EQ, status, ctx.i32_type().const_zero(), "ok")
        .llvm_ctx("check host call status")?;
    builder
        .build_conditional_branch(ok, success, failure)
        .llvm_ctx("branch on host call status")?;
    builder.position_at_end(success);
    let value = builder
        .build_load(ptr, result, "value")
        .llvm_ctx("load successful host result")?
        .into_pointer_value();
    let value = call_pointer(emitter, &builder, "hew_host_text_from_owned", value)?;
    builder
        .build_store(out, value)
        .llvm_ctx("publish host result")?;
    builder
        .build_return(Some(&ctx.i32_type().const_zero()))
        .llvm_ctx("return host success")?;
    builder.position_at_end(failure);
    let value = builder
        .build_load(ptr, fault, "failure.fault")
        .llvm_ctx("load owned host fault")?
        .into_pointer_value();
    let value = call_pointer(emitter, &builder, "hew_fault_into_host_error", value)?;
    builder
        .build_store(error, value)
        .llvm_ctx("publish owned host error")?;
    let code = get_or_declare_external(
        &emitter.llvm,
        "hew_host_error_code",
        ctx.i32_type().fn_type(&[ptr.into()], false),
    )?;
    let code = builder
        .build_call(code, &[value.into()], "error.code")
        .llvm_ctx("read public error code")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("host error code returned void".into()))?;
    builder
        .build_return(Some(&code))
        .llvm_ctx("return host failure")?;
    Ok(())
}

fn call_pointer<'ctx>(
    emitter: &ModuleEmitter<'ctx, '_>,
    builder: &Builder<'ctx>,
    symbol: &str,
    value: PointerValue<'ctx>,
) -> CodegenResult<PointerValue<'ctx>> {
    let function = external_unary_ptr(emitter.ctx, &emitter.llvm, symbol)?;
    builder
        .build_call(function, &[value.into()], "host.owner")
        .llvm_ctx("adapt host owner")?
        .try_as_basic_value()
        .basic()
        .map(BasicValueEnum::into_pointer_value)
        .ok_or_else(|| CodegenError::FailClosed("host owner adapter returned void".into()))
}
