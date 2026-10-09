//! Plain continuation frames; semantic cleanup edges come from physical MIR.

use super::*;
use inkwell::attributes::AttributeLoc;
use inkwell::values::{AsValueRef, BasicValue, InstructionValue};
use std::cell::{Cell, RefCell};

pub(super) struct Frame<'ctx> {
    pub state: PointerValue<'ctx>,
    pub destroying: PointerValue<'ctx>,
    pub finish: BasicBlock<'ctx>,
    pub allocations: BasicBlock<'ctx>,
    index: PointerValue<'ctx>,
    dispatch: InstructionValue<'ctx>,
    next_state: Cell<u32>,
    exit: BasicBlock<'ctx>,
    invalid_destroy: BasicBlock<'ctx>,
    carried: RefCell<BTreeSet<usize>>,
}

const _: () = {
    assert!(std::mem::offset_of!(hew_runtime::cont::CoroFramePrefix, resume) == 0);
    assert!(
        std::mem::offset_of!(hew_runtime::cont::CoroFramePrefix, destroy)
            == std::mem::size_of::<*const ()>()
    );
};

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
    function.add_attribute(
        AttributeLoc::Function,
        ctx.create_string_attribute("hew.resumable", ""),
    );
    let initial = function.get_first_basic_block().ok_or_else(|| {
        CodegenError::FailClosed("resumable function lacks its initial block".into())
    })?;
    let insertion = builder.get_insert_block().ok_or_else(|| {
        CodegenError::FailClosed("resumable function lacks an insertion point".into())
    })?;
    let handle = builder
        .build_alloca(ctx.i8_type(), "frame.base")
        .llvm_ctx("reserve continuation base")?;
    mark(ctx, handle, "hew.frame.base")?;
    let index = builder
        .build_alloca(ctx.i32_type(), "frame.index")
        .llvm_ctx("reserve continuation state")?;
    mark(ctx, index, "hew.frame.index")?;
    let destroying = builder
        .build_alloca(ctx.bool_type(), "frame.destroying")
        .llvm_ctx("reserve destruction entry")?;
    mark(ctx, destroying, "hew.frame.destroying")?;
    builder
        .build_store(index, ctx.i32_type().const_zero())
        .llvm_ctx("initialize continuation state")?;
    builder
        .build_store(destroying, ctx.bool_type().const_zero())
        .llvm_ctx("initialize destruction entry")?;
    let allocations = ctx.append_basic_block(function, "frame.dispatch");
    let invalid = ctx.append_basic_block(function, "frame.invalid");
    let finish = ctx.append_basic_block(function, "frame.finish");
    let final_block = ctx.append_basic_block(function, "frame.final");
    let cleanup = ctx.append_basic_block(function, "frame.cleanup");
    let exit = ctx.append_basic_block(function, "frame.exit");
    let invalid_destroy = ctx.append_basic_block(function, "frame.invalid.destroy");
    builder.position_at_end(allocations);
    let selected = builder
        .build_load(ctx.i32_type(), index, "frame.resume.index")
        .llvm_ctx("read continuation state")?
        .into_int_value();
    let dispatch = builder
        .build_switch(
            selected,
            invalid,
            &[
                (ctx.i32_type().const_zero(), initial),
                (ctx.i32_type().const_int(1, false), cleanup),
            ],
        )
        .llvm_ctx("dispatch continuation entry")?;
    builder.position_at_end(finish);
    let is_destroying = builder
        .build_load(ctx.bool_type(), destroying, "frame.is.destroying")
        .llvm_ctx("inspect destruction entry")?
        .into_int_value();
    builder
        .build_conditional_branch(is_destroying, cleanup, final_block)
        .llvm_ctx("settle continuation")?;
    builder.position_at_end(final_block);
    builder
        .build_store(handle, ctx.ptr_type(AddressSpace::default()).const_null())
        .llvm_ctx("publish continuation completion")?;
    builder
        .build_store(index, ctx.i32_type().const_int(1, false))
        .llvm_ctx("retain terminal continuation state")?;
    builder
        .build_unconditional_branch(exit)
        .llvm_ctx("return completed continuation")?;
    builder.position_at_end(cleanup);
    let free = external(
        module,
        "hew_cont_frame_free",
        ctx.void_type()
            .fn_type(&[ctx.ptr_type(AddressSpace::default()).into()], false),
    )?;
    builder
        .build_call(free, &[handle.into()], "")
        .llvm_ctx("release completed continuation frame")?;
    builder
        .build_unconditional_branch(exit)
        .llvm_ctx("finish continuation destruction")?;
    builder.position_at_end(exit);
    builder
        .build_return(Some(&handle))
        .llvm_ctx("return continuation to owner")?;
    let abort = external(module, "abort", ctx.void_type().fn_type(&[], false))?;
    for block in [invalid, invalid_destroy] {
        builder.position_at_end(block);
        builder
            .build_call(abort, &[], "")
            .llvm_ctx("refuse invalid continuation entry")?;
        builder
            .build_unreachable()
            .llvm_ctx("terminate invalid continuation entry")?;
    }
    builder.position_at_end(insertion);
    let frame = Frame {
        state,
        destroying,
        finish,
        allocations,
        index,
        dispatch,
        next_state: Cell::new(2),
        exit,
        invalid_destroy,
        carried: RefCell::new(BTreeSet::new()),
    };
    frame.carry(ctx, builder, state, "invocation.state.slot")?;
    Ok(frame)
}

fn mark(ctx: &Context, slot: PointerValue<'_>, name: &str) -> CodegenResult<()> {
    slot.as_instruction()
        .ok_or_else(|| CodegenError::FailClosed("frame field lacks its instruction".into()))?
        .set_metadata(ctx.metadata_node(&[]), ctx.get_kind_id(name))
        .map_err(|error| CodegenError::FailClosed(error.to_string()))
}

impl<'ctx> Frame<'ctx> {
    pub fn carry<T: inkwell::values::BasicValue<'ctx> + Copy>(
        &self,
        ctx: &'ctx Context,
        builder: &Builder<'ctx>,
        value: T,
        name: &str,
    ) -> CodegenResult<T> {
        let basic = value.as_basic_value_enum();
        if basic.as_instruction_value().is_none()
            || !self
                .carried
                .borrow_mut()
                .insert(value.as_value_ref() as usize)
        {
            return Ok(value);
        }
        let slot = self.storage(ctx, basic.get_type(), name)?;
        builder
            .build_store(slot, value)
            .llvm_ctx("retain emitter suspension carrier")?
            .set_metadata(ctx.metadata_node(&[]), ctx.get_kind_id("hew.carry"))
            .map_err(|error| CodegenError::FailClosed(error.to_string()))?;
        Ok(value)
    }

    pub fn storage(
        &self,
        ctx: &'ctx Context,
        ty: BasicTypeEnum<'ctx>,
        name: &str,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let builder = ctx.create_builder();
        let mut first = self.allocations.get_first_instruction();
        while first.is_some_and(|instruction| {
            instruction.get_opcode() == inkwell::values::InstructionOpcode::Phi
        }) {
            first = first.and_then(inkwell::values::InstructionValue::get_next_instruction);
        }
        if let Some(first) = first {
            builder.position_before(&first);
        } else {
            builder.position_at_end(self.allocations);
        }
        builder
            .build_alloca(ty, name)
            .llvm_ctx("allocate frame carrier")
    }

    pub fn stack(&self, ctx: &Context, slot: PointerValue<'ctx>) -> CodegenResult<()> {
        mark(ctx, slot, "hew.stack")
    }

    pub fn suspend(
        &self,
        ctx: &'ctx Context,
        builder: &Builder<'ctx>,
        resumed: BasicBlock<'ctx>,
        destroyed: BasicBlock<'ctx>,
    ) -> CodegenResult<()> {
        let next = self.next_state.get();
        self.next_state
            .set(next.checked_add(1).ok_or_else(|| {
                CodegenError::FailClosed("continuation state exceeds u32".into())
            })?);
        let function = self.allocations.get_parent().ok_or_else(|| {
            CodegenError::FailClosed("continuation dispatch has no function".into())
        })?;
        let arm = ctx.append_basic_block(function, "frame.resume");
        let entry_builder = ctx.create_builder();
        entry_builder.position_at_end(arm);
        let destroying = entry_builder
            .build_load(ctx.bool_type(), self.destroying, "frame.via.destroy")
            .llvm_ctx("inspect continuation entry kind")?
            .into_int_value();
        entry_builder
            .build_conditional_branch(destroying, destroyed, resumed)
            .llvm_ctx("select continuation entry kind")?;
        // SAFETY: dispatch is a live i32 switch and arm belongs to its function.
        unsafe {
            inkwell::llvm_sys::core::LLVMAddCase(
                self.dispatch.as_value_ref(),
                ctx.i32_type()
                    .const_int(u64::from(next), false)
                    .as_value_ref(),
                arm.as_mut_ptr(),
            );
        }
        builder
            .build_store(self.index, ctx.i32_type().const_int(u64::from(next), false))
            .llvm_ctx("retain continuation resume point")?;
        let destroying = builder
            .build_load(ctx.bool_type(), self.destroying, "frame.suspend.destroying")
            .llvm_ctx("guard synchronous destruction")?
            .into_int_value();
        builder
            .build_conditional_branch(destroying, self.invalid_destroy, self.exit)
            .llvm_ctx("return only a resumable continuation")?;
        Ok(())
    }
}

pub(super) fn lower<'ctx>(
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    machine: &TargetMachine,
) -> CodegenResult<()> {
    let functions: Vec<_> = module
        .get_functions()
        .filter(|function| {
            function
                .get_string_attribute(AttributeLoc::Function, "hew.resumable")
                .is_some()
        })
        .collect();
    for function in functions {
        super::frames::lower(ctx, module, &machine.get_target_data(), function)?;
    }
    Ok(())
}
