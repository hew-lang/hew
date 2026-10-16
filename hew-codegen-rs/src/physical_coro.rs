//! LLVM switched-resume coroutines; physical MIR owns semantic cleanup edges.

use super::suspend::call_value;
use super::*;
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::context::AsContextRef;
use inkwell::intrinsics::Intrinsic;
use inkwell::llvm_sys::{core::LLVMBuildCall2, prelude::LLVMValueRef};
use inkwell::types::AsTypeRef;
use inkwell::values::{AsValueRef, CallSiteValue};
use std::cell::Cell;

pub(super) struct Frame<'ctx> {
    pub state: PointerValue<'ctx>,
    pub budget: PointerValue<'ctx>,
    destroying: PointerValue<'ctx>,
    pub finish: BasicBlock<'ctx>,
    entry: BasicBlock<'ctx>,
    suspend: FunctionValue<'ctx>,
    exit: BasicBlock<'ctx>,
    invalid_destroy: BasicBlock<'ctx>,
    child: Cell<Option<(PointerValue<'ctx>, PointerValue<'ctx>)>>,
    child_invocation: Cell<Option<PointerValue<'ctx>>>,
}

const _: () = {
    assert!(std::mem::offset_of!(hew_runtime::cont::CoroFramePrefix, resume) == 0);
    assert!(
        std::mem::offset_of!(hew_runtime::cont::CoroFramePrefix, destroy)
            == std::mem::size_of::<*const ()>()
    );
};

fn intrinsic<'ctx>(
    module: &Module<'ctx>,
    name: &str,
    overload: &[BasicTypeEnum<'ctx>],
) -> CodegenResult<FunctionValue<'ctx>> {
    Intrinsic::find(name)
        .and_then(|intrinsic| intrinsic.get_declaration(module, overload))
        .ok_or_else(|| CodegenError::FailClosed(format!("missing LLVM intrinsic {name}")))
}

// Inkwell's basic values exclude tokens. Confine the raw API to calls carrying
// coroutine identity and suspension tokens.
fn token_call<'ctx>(
    builder: &Builder<'ctx>,
    function: FunctionValue<'ctx>,
    args: &mut [LLVMValueRef],
) -> CallSiteValue<'ctx> {
    // SAFETY: callers supply live values with the intrinsic's declared types;
    // the builder is positioned in the function being emitted.
    unsafe {
        CallSiteValue::new(LLVMBuildCall2(
            builder.as_mut_ptr(),
            function.get_type().as_type_ref(),
            function.as_value_ref(),
            args.as_mut_ptr(),
            u32::try_from(args.len()).expect("coroutine intrinsic argument count fits u32"),
            c"".as_ptr(),
        ))
    }
}

pub(super) fn entry_storage<'ctx>(
    ctx: &'ctx Context,
    entry: BasicBlock<'ctx>,
    ty: BasicTypeEnum<'ctx>,
    name: &str,
) -> CodegenResult<PointerValue<'ctx>> {
    let builder = ctx.create_builder();
    if let Some(first) = entry.get_first_instruction() {
        builder.position_before(&first);
    } else {
        builder.position_at_end(entry);
    }
    builder
        .build_alloca(ty, name)
        .llvm_ctx("allocate local storage")
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
        ctx.create_enum_attribute(Attribute::get_named_enum_kind_id("presplitcoroutine"), 0),
    );
    let entry = function
        .get_first_basic_block()
        .ok_or_else(|| CodegenError::FailClosed("coroutine lacks an entry block".into()))?;
    let pointer = ctx.ptr_type(AddressSpace::default());
    let null = pointer.const_null();
    let destroying = entry_storage(ctx, entry, ctx.bool_type().into(), "frame.destroying")?;
    builder
        .build_store(destroying, ctx.bool_type().const_zero())
        .llvm_ctx("initialize destruction entry")?;
    let id = token_call(
        builder,
        intrinsic(module, "llvm.coro.id", &[])?,
        &mut [
            ctx.i32_type().const_int(16, false).as_value_ref(),
            null.as_value_ref(),
            null.as_value_ref(),
            null.as_value_ref(),
        ],
    );
    let alloc = token_call(
        builder,
        intrinsic(module, "llvm.coro.alloc", &[])?,
        &mut [id.as_value_ref()],
    )
    .try_as_basic_value()
    .basic()
    .unwrap()
    .into_int_value();
    let initial = builder.get_insert_block().unwrap();
    let allocate = ctx.append_basic_block(function, "coro.allocate");
    let start = ctx.append_basic_block(function, "coro.begin");
    builder
        .build_conditional_branch(alloc, allocate, start)
        .llvm_ctx("select coroutine allocation")?;
    builder.position_at_end(allocate);
    let size = call_value(
        builder,
        intrinsic(module, "llvm.coro.size", &[ctx.i64_type().into()])?,
        &[],
        "frame.size",
    )?;
    let allocator = get_or_declare_external(
        module,
        "hew_cont_frame_alloc",
        pointer.fn_type(&[ctx.i64_type().into()], false),
    )?;
    let memory =
        call_value(builder, allocator, &[size.into()], "frame.memory")?.into_pointer_value();
    builder
        .build_unconditional_branch(start)
        .llvm_ctx("initialize coroutine")?;
    builder.position_at_end(start);
    let storage = builder
        .build_phi(pointer, "frame.storage")
        .llvm_ctx("select coroutine storage")?;
    storage.add_incoming(&[(&null, initial), (&memory, allocate)]);
    let handle = token_call(
        builder,
        intrinsic(module, "llvm.coro.begin", &[])?,
        &mut [id.as_value_ref(), storage.as_basic_value().as_value_ref()],
    )
    .try_as_basic_value()
    .basic()
    .unwrap()
    .into_pointer_value();
    let budget_fn = get_or_declare_external(
        module,
        "hew_coro_safepoint_budget",
        pointer.fn_type(&[pointer.into(), ctx.i32_type().into()], false),
    )?;
    let budget = call_value(
        builder,
        budget_fn,
        &[
            state.into(),
            ctx.i32_type()
                .const_int(u64::from(hew_mir::SAFEPOINT_BUDGET), false)
                .into(),
        ],
        "safepoint.budget",
    )?
    .into_pointer_value();
    let body = ctx.append_basic_block(function, "coro.body");
    builder
        .build_unconditional_branch(body)
        .llvm_ctx("enter coroutine body")?;
    let finish = ctx.append_basic_block(function, "coro.finish");
    let final_block = ctx.append_basic_block(function, "coro.final");
    let cleanup = ctx.append_basic_block(function, "coro.cleanup");
    let free_block = ctx.append_basic_block(function, "coro.free");
    let exit = ctx.append_basic_block(function, "coro.exit");
    let invalid_destroy = ctx.append_basic_block(function, "coro.invalid.destroy");
    let suspend = intrinsic(module, "llvm.coro.suspend", &[])?;
    // SAFETY: the token constant belongs to this context.
    let none = unsafe {
        inkwell::llvm_sys::core::LLVMConstNull(inkwell::llvm_sys::core::LLVMTokenTypeInContext(
            ctx.as_ctx_ref(),
        ))
    };
    builder.position_at_end(finish);
    let is_destroying = builder
        .build_load(ctx.bool_type(), destroying, "frame.is.destroying")
        .llvm_ctx("inspect destruction entry")?
        .into_int_value();
    builder
        .build_conditional_branch(is_destroying, cleanup, final_block)
        .llvm_ctx("settle coroutine")?;
    builder.position_at_end(final_block);
    let outcome = token_call(
        builder,
        suspend,
        &mut [none, ctx.bool_type().const_int(1, false).as_value_ref()],
    )
    .try_as_basic_value()
    .basic()
    .unwrap()
    .into_int_value();
    builder
        .build_switch(
            outcome,
            exit,
            &[
                (ctx.i8_type().const_zero(), invalid_destroy),
                (ctx.i8_type().const_int(1, false), cleanup),
            ],
        )
        .llvm_ctx("finish coroutine")?;
    builder.position_at_end(cleanup);
    let memory = token_call(
        builder,
        intrinsic(module, "llvm.coro.free", &[])?,
        &mut [id.as_value_ref(), handle.as_value_ref()],
    )
    .try_as_basic_value()
    .basic()
    .unwrap()
    .into_pointer_value();
    let allocated = builder
        .build_is_not_null(memory, "frame.allocated")
        .llvm_ctx("test heap frame")?;
    builder
        .build_conditional_branch(allocated, free_block, exit)
        .llvm_ctx("release only heap storage")?;
    builder.position_at_end(free_block);
    let free = get_or_declare_external(
        module,
        "hew_cont_frame_free",
        ctx.void_type().fn_type(&[pointer.into()], false),
    )?;
    builder
        .build_call(free, &[memory.into()], "")
        .llvm_ctx("free coroutine storage")?;
    builder
        .build_unconditional_branch(exit)
        .llvm_ctx("finish destruction")?;
    builder.position_at_end(exit);
    token_call(
        builder,
        intrinsic(module, "llvm.coro.end", &[])?,
        &mut [
            handle.as_value_ref(),
            ctx.bool_type().const_zero().as_value_ref(),
            none,
        ],
    );
    builder
        .build_return(Some(&handle))
        .llvm_ctx("return coroutine handle")?;
    builder.position_at_end(invalid_destroy);
    let abort = get_or_declare_external(module, "abort", ctx.void_type().fn_type(&[], false))?;
    builder
        .build_call(abort, &[], "")
        .llvm_ctx("refuse invalid coroutine entry")?;
    builder
        .build_unreachable()
        .llvm_ctx("terminate invalid coroutine entry")?;
    builder.position_at_end(body);
    Ok(Frame {
        state,
        budget,
        destroying,
        finish,
        entry,
        suspend,
        exit,
        invalid_destroy,
        child: Cell::new(None),
        child_invocation: Cell::new(None),
    })
}

impl<'ctx> Frame<'ctx> {
    pub fn init_child(
        &self,
        ctx: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let pointer = ctx.ptr_type(AddressSpace::default());
        let storage = if let Some(storage) = self.child_invocation.get() {
            storage
        } else {
            // The exact HewCoroInvocation fields; LLVM applies the target's C
            // padding and alignment rather than importing the host's size.
            let record = ctx.struct_type(
                &[
                    pointer.into(),
                    ctx.i32_type().into(),
                    ctx.i32_type().into(),
                    ctx.i32_type().into(),
                    ctx.i32_type().into(),
                    ctx.i8_type().into(),
                    ctx.i8_type().into(),
                    ctx.i8_type().into(),
                ],
                false,
            );
            let storage = self.storage(ctx, record.into(), "call.child.invocation")?;
            self.child_invocation.set(Some(storage));
            storage
        };
        let init = get_or_declare_external(
            module,
            "hew_coro_state_init_child",
            ctx.void_type().fn_type(&[pointer.into(); 2], false),
        )?;
        builder
            .build_call(init, &[storage.into(), self.state.into()], "")
            .llvm_ctx("initialize the frame-owned child invocation")?;
        Ok(storage)
    }

    pub fn child_storage(
        &self,
        ctx: &'ctx Context,
    ) -> CodegenResult<(PointerValue<'ctx>, PointerValue<'ctx>)> {
        if let Some(slots) = self.child.get() {
            return Ok(slots);
        }
        let pointer = ctx.ptr_type(AddressSpace::default());
        let slots = (
            self.storage(ctx, pointer.into(), "call.child.state")?,
            self.storage(ctx, pointer.into(), "call.child.frame")?,
        );
        self.child.set(Some(slots));
        Ok(slots)
    }

    pub fn storage(
        &self,
        ctx: &'ctx Context,
        ty: BasicTypeEnum<'ctx>,
        name: &str,
    ) -> CodegenResult<PointerValue<'ctx>> {
        entry_storage(ctx, self.entry, ty, name)
    }

    pub fn suspend(
        &self,
        ctx: &'ctx Context,
        builder: &Builder<'ctx>,
        resumed: BasicBlock<'ctx>,
        destroyed: BasicBlock<'ctx>,
    ) -> CodegenResult<()> {
        let function = self.entry.get_parent().unwrap();
        let suspend = ctx.append_basic_block(function, "coro.suspend");
        let destroy = ctx.append_basic_block(function, "coro.destroy");
        let destroying = builder
            .build_load(ctx.bool_type(), self.destroying, "frame.suspend.destroying")
            .llvm_ctx("guard synchronous destruction")?
            .into_int_value();
        builder
            .build_conditional_branch(destroying, self.invalid_destroy, suspend)
            .llvm_ctx("permit suspension only during resume")?;
        builder.position_at_end(suspend);
        // SAFETY: the token constant belongs to this context.
        let none = unsafe {
            inkwell::llvm_sys::core::LLVMConstNull(inkwell::llvm_sys::core::LLVMTokenTypeInContext(
                ctx.as_ctx_ref(),
            ))
        };
        let outcome = token_call(
            builder,
            self.suspend,
            &mut [none, ctx.bool_type().const_zero().as_value_ref()],
        )
        .try_as_basic_value()
        .basic()
        .unwrap()
        .into_int_value();
        builder
            .build_switch(
                outcome,
                self.exit,
                &[
                    (ctx.i8_type().const_zero(), resumed),
                    (ctx.i8_type().const_int(1, false), destroy),
                ],
            )
            .llvm_ctx("dispatch coroutine suspension")?;
        builder.position_at_end(destroy);
        builder
            .build_store(self.destroying, ctx.bool_type().const_int(1, false))
            .llvm_ctx("enter synchronous destruction")?;
        builder
            .build_unconditional_branch(destroyed)
            .llvm_ctx("enter semantic cleanup")?;
        Ok(())
    }
}

pub(super) fn handle_operation<'ctx>(
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    handle: PointerValue<'ctx>,
    name: &str,
) -> CodegenResult<()> {
    let function = builder
        .get_insert_block()
        .and_then(|block| block.get_parent())
        .unwrap();
    let invoke = ctx.append_basic_block(function, "coro.handle.invoke");
    let done = ctx.append_basic_block(function, "coro.handle.done");
    let present = builder
        .build_is_not_null(handle, "coro.handle.present")
        .llvm_ctx("test optional coroutine handle")?;
    builder
        .build_conditional_branch(present, invoke, done)
        .llvm_ctx("select coroutine operation")?;
    builder.position_at_end(invoke);
    builder
        .build_call(intrinsic(module, name, &[])?, &[handle.into()], "")
        .llvm_ctx("operate on coroutine")?;
    builder
        .build_unconditional_branch(done)
        .llvm_ctx("finish coroutine operation")?;
    builder.position_at_end(done);
    Ok(())
}
