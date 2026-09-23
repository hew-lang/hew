//! LLVM coroutine mechanics; semantic cleanup edges are supplied by physical MIR.

use super::*;
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::llvm_sys::core::{
    LLVMAddCase, LLVMBuildCall2, LLVMBuildSwitch, LLVMConstNull, LLVMTokenTypeInContext,
};
use inkwell::llvm_sys::prelude::LLVMValueRef;
use inkwell::types::AsTypeRef;
use inkwell::values::AsValueRef;

pub(super) struct Frame<'ctx> {
    pub handle: PointerValue<'ctx>,
    pub state: PointerValue<'ctx>,
    pub destroying: PointerValue<'ctx>,
    pub finish: BasicBlock<'ctx>,
    pub allocations: BasicBlock<'ctx>,
    exit: BasicBlock<'ctx>,
}

fn intrinsic<'ctx>(
    module: &Module<'ctx>,
    name: &str,
    types: &[BasicTypeEnum<'ctx>],
) -> CodegenResult<FunctionValue<'ctx>> {
    Intrinsic::find(name)
        .and_then(|intrinsic| intrinsic.get_declaration(module, types))
        .ok_or_else(|| CodegenError::FailClosed(format!("missing LLVM intrinsic {name}")))
}

/// inkwell does not wrap LLVM token values. Keep tokens opaque and pass them
/// directly between the typed intrinsic declarations which produce/use them.
unsafe fn raw_call(
    builder: &Builder<'_>,
    function: FunctionValue<'_>,
    args: &mut [LLVMValueRef],
    name: &std::ffi::CStr,
) -> LLVMValueRef {
    // SAFETY: callers supply the exact intrinsic signature and live LLVM values.
    unsafe {
        LLVMBuildCall2(
            builder.as_mut_ptr(),
            function.get_type().as_type_ref(),
            function.as_value_ref(),
            args.as_mut_ptr(),
            args.len() as u32,
            name.as_ptr(),
        )
    }
}

pub(super) fn external<'ctx>(
    module: &Module<'ctx>,
    name: &str,
    ty: FunctionType<'ctx>,
) -> CodegenResult<FunctionValue<'ctx>> {
    if let Some(function) = module.get_function(name) {
        if function.get_type() != ty {
            return Err(CodegenError::FailClosed(format!(
                "inconsistent coroutine ABI for {name}"
            )));
        }
        Ok(function)
    } else {
        Ok(module.add_function(name, ty, Some(Linkage::External)))
    }
}

pub(super) fn begin<'ctx>(
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    function: FunctionValue<'ctx>,
    state: PointerValue<'ctx>,
) -> CodegenResult<Frame<'ctx>> {
    let attribute = Attribute::get_named_enum_kind_id("presplitcoroutine");
    if attribute == 0 {
        return Err(CodegenError::FailClosed(
            "LLVM lacks presplitcoroutine".into(),
        ));
    }
    function.add_attribute(
        AttributeLoc::Function,
        ctx.create_enum_attribute(attribute, 0),
    );
    let entry = builder
        .get_insert_block()
        .ok_or_else(|| CodegenError::FailClosed("coroutine has no entry block".into()))?;
    let allocate = ctx.append_basic_block(function, "coro.allocate");
    let body = ctx.append_basic_block(function, "coro.body");
    let pointer = ctx.ptr_type(AddressSpace::default());
    let null = pointer.const_null().as_value_ref();
    let id_fn = intrinsic(module, "llvm.coro.id", &[])?;
    // SAFETY: coro.id takes i32 plus three pointers and returns a token. Its
    // promise is null; explicit result/fault slots carry all published values.
    let id = unsafe {
        raw_call(
            builder,
            id_fn,
            &mut [ctx.i32_type().const_zero().as_value_ref(), null, null, null],
            c"coro.id",
        )
    };
    let alloc_fn = intrinsic(module, "llvm.coro.alloc", &[])?;
    // SAFETY: coro.alloc consumes the id token and returns i1.
    let needs_alloc =
        unsafe { IntValue::new(raw_call(builder, alloc_fn, &mut [id], c"coro.needs.alloc")) };
    builder
        .build_conditional_branch(needs_alloc, allocate, body)
        .llvm_ctx("select coroutine allocation")?;
    builder.position_at_end(allocate);
    let size_fn = intrinsic(module, "llvm.coro.size", &[ctx.i64_type().into()])?;
    let size = builder
        .build_call(size_fn, &[], "coro.size")
        .llvm_ctx("measure coroutine frame")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("coro.size has no result".into()))?;
    let alloc = external(
        module,
        "hew_cont_frame_alloc",
        pointer.fn_type(&[ctx.i64_type().into()], false),
    )?;
    let memory = builder
        .build_call(alloc, &[size.into()], "coro.memory")
        .llvm_ctx("allocate coroutine frame")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("frame allocation has no result".into()))?
        .into_pointer_value();
    builder
        .build_unconditional_branch(body)
        .llvm_ctx("enter coroutine body")?;
    builder.position_at_end(body);
    let allocation = builder
        .build_phi(pointer, "coro.allocation")
        .llvm_ctx("select frame memory")?;
    allocation.add_incoming(&[(&pointer.const_null(), entry), (&memory, allocate)]);
    let begin_fn = intrinsic(module, "llvm.coro.begin", &[])?;
    // SAFETY: coro.begin consumes the id token and allocation pointer, returns ptr.
    let handle = unsafe {
        PointerValue::new(raw_call(
            builder,
            begin_fn,
            &mut [id, allocation.as_basic_value().as_value_ref()],
            c"coro.handle",
        ))
    };
    let destroying = builder
        .build_alloca(ctx.bool_type(), "coro.destroying")
        .llvm_ctx("allocate coroutine destruction flag")?;
    builder
        .build_store(destroying, ctx.bool_type().const_zero())
        .llvm_ctx("initialize coroutine destruction flag")?;
    let frame = Frame {
        handle,
        state,
        destroying,
        finish: ctx.append_basic_block(function, "coro.finish"),
        allocations: body,
        exit: ctx.append_basic_block(function, "coro.exit"),
    };
    let final_suspend = ctx.append_basic_block(function, "coro.final");
    let invalid_resume = ctx.append_basic_block(function, "coro.invalid.resume");
    let cleanup = ctx.append_basic_block(function, "coro.cleanup");
    let free = ctx.append_basic_block(function, "coro.free");
    builder.position_at_end(frame.finish);
    let is_destroying = builder
        .build_load(ctx.bool_type(), destroying, "coro.is.destroying")
        .llvm_ctx("load destruction phase")?
        .into_int_value();
    builder
        .build_conditional_branch(is_destroying, cleanup, final_suspend)
        .llvm_ctx("finish coroutine execution")?;
    builder.position_at_end(final_suspend);
    frame.suspend(ctx, module, builder, invalid_resume, cleanup, true)?;
    builder.position_at_end(invalid_resume);
    builder
        .build_unreachable()
        .llvm_ctx("refuse final coroutine resume")?;
    builder.position_at_end(cleanup);
    let free_fn = intrinsic(module, "llvm.coro.free", &[])?;
    // SAFETY: coro.free consumes the original id token and live frame pointer.
    let memory = unsafe {
        PointerValue::new(raw_call(
            builder,
            free_fn,
            &mut [id, handle.as_value_ref()],
            c"coro.free.memory",
        ))
    };
    let absent = builder
        .build_is_null(memory, "coro.free.absent")
        .llvm_ctx("test elided frame")?;
    builder
        .build_conditional_branch(absent, frame.exit, free)
        .llvm_ctx("select frame release")?;
    builder.position_at_end(free);
    let dealloc = external(
        module,
        "hew_cont_frame_free",
        ctx.void_type().fn_type(&[pointer.into()], false),
    )?;
    builder
        .build_call(dealloc, &[memory.into()], "")
        .llvm_ctx("release coroutine frame")?;
    builder
        .build_unconditional_branch(frame.exit)
        .llvm_ctx("finish frame release")?;
    builder.position_at_end(frame.exit);
    let end = intrinsic(module, "llvm.coro.end", &[])?;
    // SAFETY: the one common exit owns the coroutine's fallthrough coro.end.
    // LLVM's final operand is the token-none constant, not a pointer.
    unsafe {
        let none = LLVMConstNull(LLVMTokenTypeInContext(ctx.raw()));
        raw_call(
            builder,
            end,
            &mut [
                handle.as_value_ref(),
                ctx.bool_type().const_zero().as_value_ref(),
                none,
            ],
            c"",
        );
    }
    builder
        .build_return(Some(&handle))
        .llvm_ctx("return continuation handle")?;
    builder.position_at_end(body);
    Ok(frame)
}

impl<'ctx> Frame<'ctx> {
    pub fn suspend(
        &self,
        ctx: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
        resumed: BasicBlock<'ctx>,
        destroyed: BasicBlock<'ctx>,
        is_final: bool,
    ) -> CodegenResult<()> {
        let save_fn = intrinsic(module, "llvm.coro.save", &[])?;
        let suspend_fn = intrinsic(module, "llvm.coro.suspend", &[])?;
        // SAFETY: LLVM tokens pass only from coro.save to coro.suspend. The
        // returned i8 selects resumed(0), destroyed(1), or return-to-owner.
        unsafe {
            let save = raw_call(
                builder,
                save_fn,
                &mut [self.handle.as_value_ref()],
                c"coro.save",
            );
            let status = raw_call(
                builder,
                suspend_fn,
                &mut [
                    save,
                    ctx.bool_type()
                        .const_int(u64::from(is_final), false)
                        .as_value_ref(),
                ],
                c"coro.suspend",
            );
            let switch = LLVMBuildSwitch(builder.as_mut_ptr(), status, self.exit.as_mut_ptr(), 2);
            LLVMAddCase(
                switch,
                ctx.i8_type().const_zero().as_value_ref(),
                resumed.as_mut_ptr(),
            );
            LLVMAddCase(
                switch,
                ctx.i8_type().const_int(1, false).as_value_ref(),
                destroyed.as_mut_ptr(),
            );
        }
        Ok(())
    }
}

pub(super) fn lower(module: &Module<'_>, machine: &TargetMachine) -> CodegenResult<()> {
    module
        .run_passes(
            "globaldce,coro-early,cgscc(coro-split),coro-cleanup",
            machine,
            inkwell::passes::PassBuilderOptions::create(),
        )
        .map_err(|error| CodegenError::Llvm(format!("coroutine lowering failed: {error}")))?;
    module
        .verify()
        .map_err(|error| CodegenError::LlvmVerify(format!("coroutine lowering: {error}")))
}

#[cfg(test)]
mod abi_tests {
    use super::*;
    use hew_runtime::cont::CoroFramePrefix;
    use inkwell::values::{AnyValue, BasicValue, InstructionOpcode};
    use std::mem::offset_of;

    /// Split one suspending coroutine and return the frame offsets its ramp
    /// stores the `.resume` and `.destroy` outlines at.
    fn split_prefix_offsets() -> (u64, u64) {
        let triple = crate::llvm::native_emission_triple();
        let machine = crate::llvm::target_machine_for_triple_with_opt_level(
            &triple,
            crate::llvm::OptLevel::O0,
        )
        .unwrap();
        let target = machine.get_target_data();
        let ctx = Context::create();
        let module = ctx.create_module("frame_prefix");
        module.set_triple(&machine.get_triple());
        module.set_data_layout(&target.get_data_layout());
        let pointer = ctx.ptr_type(AddressSpace::default());
        let function =
            module.add_function("probe", pointer.fn_type(&[pointer.into()], false), None);
        let builder = ctx.create_builder();
        builder.position_at_end(ctx.append_basic_block(function, "entry"));
        let state = function.get_first_param().unwrap().into_pointer_value();
        let frame = begin(&ctx, &module, &builder, function, state).unwrap();
        let resumed = ctx.append_basic_block(function, "resumed");
        let destroyed = ctx.append_basic_block(function, "destroyed");
        frame
            .suspend(&ctx, &module, &builder, resumed, destroyed, false)
            .unwrap();
        builder.position_at_end(resumed);
        builder.build_unconditional_branch(frame.finish).unwrap();
        builder.position_at_end(destroyed);
        builder
            .build_store(frame.destroying, ctx.bool_type().const_int(1, false))
            .unwrap();
        builder.build_unconditional_branch(frame.finish).unwrap();
        lower(&module, &machine).unwrap();

        let layout = module
            .get_struct_type("probe.Frame")
            .expect("CoroSplit names the switched-resume frame");
        let outline = |suffix: &str| {
            module
                .get_function(&format!("probe.{suffix}"))
                .unwrap()
                .as_global_value()
                .as_pointer_value()
        };
        let (resume, destroy) = (outline("resume"), outline("destroy"));
        let mut slots = (None, None);
        let ramp = module.get_function("probe").unwrap();
        for block in ramp.get_basic_blocks() {
            for store in block.get_instructions() {
                if store.get_opcode() != InstructionOpcode::Store {
                    continue;
                }
                let stored = store.get_operand(0).unwrap().value().unwrap();
                let address = store.get_operand(1).unwrap().value().unwrap();
                // Slot zero is the frame base itself; a later slot is a
                // constant struct GEP whose last index names the field.
                let offset = match address.as_instruction_value() {
                    Some(gep) if gep.get_opcode() == InstructionOpcode::GetElementPtr => {
                        let last = gep.get_num_operands() - 1;
                        let field = gep
                            .get_operand(last)
                            .unwrap()
                            .value()
                            .unwrap()
                            .into_int_value()
                            .get_zero_extended_constant()
                            .unwrap();
                        target
                            .offset_of_element(&layout, u32::try_from(field).unwrap())
                            .unwrap()
                    }
                    _ => 0,
                };
                // A frame that may be elided stores `select(alloc, destroy,
                // cleanup)`: the heap-frame arm is the destroy outline.
                let stored = match stored.as_instruction_value() {
                    Some(select) if select.get_opcode() == InstructionOpcode::Select => {
                        select.get_operand(1).unwrap().value().unwrap()
                    }
                    _ => stored,
                }
                .as_any_value_enum();
                if stored == resume.as_any_value_enum() {
                    slots.0 = Some(offset);
                } else if stored == destroy.as_any_value_enum() {
                    slots.1 = Some(offset);
                }
            }
        }
        (
            slots.0.expect("the ramp stores the resume outline"),
            slots.1.expect("the ramp stores the destroy outline"),
        )
    }

    /// The runtime drives continuations through `CoroFramePrefix`, so its
    /// fields must sit exactly where LLVM's split ramp stores the outlines.
    #[test]
    fn split_frame_prefix_matches_the_runtime_continuation_abi() {
        let runtime = (
            offset_of!(CoroFramePrefix, resume) as u64,
            offset_of!(CoroFramePrefix, destroy) as u64,
        );
        assert_eq!(split_prefix_offsets(), runtime);
    }

    /// A prefix that swapped its two slots would read the destroy outline as
    /// resume; the measured frame tells the two orders apart.
    #[test]
    fn a_swapped_frame_prefix_is_caught() {
        let swapped = (
            offset_of!(CoroFramePrefix, destroy) as u64,
            offset_of!(CoroFramePrefix, resume) as u64,
        );
        assert_ne!(split_prefix_offsets(), swapped);
    }
}
