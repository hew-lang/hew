//! LLVM switched-resume coroutines; physical MIR owns semantic cleanup edges.

use super::suspend::call_value;
use super::*;
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::context::AsContextRef;
use inkwell::intrinsics::Intrinsic;
use inkwell::llvm_sys::{core::LLVMBuildCall2, prelude::LLVMValueRef};
use inkwell::types::AsTypeRef;
use inkwell::values::{AsValueRef, BasicValue, CallSiteValue};
use std::cell::Cell;

pub(super) struct Frame<'ctx> {
    pub state: PointerValue<'ctx>,
    /// An authored close executes in private cleanup ancestry but completes
    /// the invocation supplied by its caller.
    pub completion_state: PointerValue<'ctx>,
    pub budget: PointerValue<'ctx>,
    destroying: PointerValue<'ctx>,
    pub finish: BasicBlock<'ctx>,
    entry: BasicBlock<'ctx>,
    suspend: FunctionValue<'ctx>,
    exit: BasicBlock<'ctx>,
    pub invalid_destroy: BasicBlock<'ctx>,
    child: Cell<Option<(PointerValue<'ctx>, PointerValue<'ctx>)>>,
    child_invocation: Cell<Option<PointerValue<'ctx>>>,
    managed_view: Cell<Option<PointerValue<'ctx>>>,
    managed_operation: Cell<Option<(PointerValue<'ctx>, PointerValue<'ctx>)>>,
    managed_frame_type: inkwell::types::StructType<'ctx>,
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

fn invocation_type(ctx: &Context) -> inkwell::types::StructType<'_> {
    // The exact HewCoroInvocation fields; LLVM applies the target's C padding
    // and alignment rather than importing the host's size.
    ctx.struct_type(
        &[
            ctx.ptr_type(AddressSpace::default()).into(),
            ctx.i32_type().into(),
            ctx.i32_type().into(),
            ctx.i32_type().into(),
            ctx.i32_type().into(),
            ctx.i8_type().into(),
            ctx.i8_type().into(),
            ctx.i8_type().into(),
            ctx.ptr_type(AddressSpace::default()).into(),
        ],
        false,
    )
}

/// Checked child ramps publish a private result on every terminal exit. Acquire
/// completion before observing outputs; after completion or synchronous child
/// destruction, the private result needs only a relaxed load.
pub(super) fn invocation_outcome<'ctx>(
    ctx: &'ctx Context,
    builder: &Builder<'ctx>,
    state: PointerValue<'ctx>,
    private: bool,
) -> CodegenResult<IntValue<'ctx>> {
    let field = builder
        .build_struct_gep(
            invocation_type(ctx),
            state,
            if private { 2 } else { 1 },
            "call.outcome.slot",
        )
        .llvm_ctx("address invocation outcome")?;
    let outcome = builder
        .build_load(ctx.i32_type(), field, "call.outcome")
        .llvm_ctx("read invocation outcome")?;
    outcome
        .as_instruction_value()
        .ok_or_else(|| CodegenError::FailClosed("invocation outcome is not a load".into()))?
        .set_atomic_ordering(if private {
            inkwell::AtomicOrdering::Monotonic
        } else {
            inkwell::AtomicOrdering::Acquire
        })
        .llvm_ctx("observe invocation publication")?;
    Ok(outcome.into_int_value())
}

pub(super) fn begin<'ctx>(
    ctx: &'ctx Context,
    target_data: &TargetData,
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
    let unbind = get_or_declare_external(
        module,
        "hew_budget_bind_frame",
        ctx.void_type().fn_type(&[pointer.into(); 2], false),
    )?;
    builder
        .build_call(unbind, &[state.into(), pointer.const_null().into()], "")
        .llvm_ctx("retire the completed invocation root view")?;
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
    let frame = Frame {
        state,
        completion_state: state,
        budget,
        destroying,
        finish,
        entry,
        suspend,
        exit,
        invalid_destroy,
        child: Cell::new(None),
        child_invocation: Cell::new(None),
        managed_view: Cell::new(None),
        managed_operation: Cell::new(None),
        managed_frame_type: managed_frame_type(ctx, target_data),
    };
    frame.bind_managed_roots(ctx, module, builder, pointer.const_null(), 0)?;
    Ok(frame)
}

impl<'ctx> Frame<'ctx> {
    /// The caller observes completion while authored cleanup uses shielded
    /// execution ancestry. Keep that private execution in the same live view.
    pub(super) fn bind_cleanup_execution(
        &self,
        ctx: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) -> CodegenResult<()> {
        let pointer = ctx.ptr_type(AddressSpace::default());
        let view_ty = self.managed_frame_type;
        let child = self.storage(ctx, pointer.into(), "close.execution.root")?;
        builder
            .build_store(child, self.state)
            .llvm_ctx("retain the authored close execution root")?;
        let view = self.storage(ctx, view_ty.into(), "close.completion.view")?;
        let row = builder
            .build_insert_value(view_ty.const_zero(), child, 2, "close.execution.view")
            .llvm_ctx("link completion to authored cleanup execution")?;
        builder
            .build_store(view, row.into_struct_value())
            .llvm_ctx("initialize the completion invocation view")?;
        let bind = get_or_declare_external(
            module,
            "hew_budget_bind_frame",
            ctx.void_type().fn_type(&[pointer.into(); 2], false),
        )?;
        builder
            .build_call(bind, &[self.completion_state.into(), view.into()], "")
            .llvm_ctx("publish authored cleanup execution to its caller")?;
        let finish = ctx.create_builder();
        finish.position_before(
            &self.finish.get_terminator().ok_or_else(|| {
                CodegenError::FailClosed("cleanup finish lacks its branch".into())
            })?,
        );
        finish
            .build_call(
                bind,
                &[self.completion_state.into(), pointer.const_null().into()],
                "",
            )
            .llvm_ctx("retire completed authored cleanup linkage")?;
        let free = get_or_declare_external(
            module,
            "hew_coro_state_free",
            ctx.void_type().fn_type(&[pointer.into()], false),
        )?;
        finish
            .build_call(free, &[self.state.into()], "")
            .llvm_ctx("release private cleanup state after retiring its root view")?;
        Ok(())
    }

    pub(super) fn bind_root_entries(
        &self,
        ctx: &'ctx Context,
        llvm: &Module<'ctx>,
        builder: &Builder<'ctx>,
        roots: &[(PointerValue<'ctx>, PointerValue<'ctx>, PointerValue<'ctx>)],
    ) -> CodegenResult<()> {
        let pointer = ctx.ptr_type(AddressSpace::default());
        let row_ty = ctx.struct_type(&[pointer.into(); 3], false);
        let count = u32::try_from(roots.len())
            .map_err(|_| CodegenError::FailClosed("too many frame roots".into()))?;
        let table_ty = row_ty.array_type(count);
        let table = self.storage(ctx, table_ty.into(), "managed.frame.slots")?;
        for (index, (layout, slot, flag)) in roots.iter().copied().enumerate() {
            // SAFETY: each index names one declared entry of the exact table.
            let row = unsafe {
                builder.build_in_bounds_gep(
                    table_ty,
                    table,
                    &[
                        ctx.i32_type().const_zero(),
                        ctx.i32_type().const_int(index as u64, false),
                    ],
                    "managed.frame.slot",
                )
            }
            .llvm_ctx("address a live frame root")?;
            for (index, entry) in [layout, slot, flag].into_iter().enumerate() {
                let field = builder
                    .build_struct_gep(
                        row_ty,
                        row,
                        u32::try_from(index).expect("three root fields"),
                        "managed.frame.field",
                    )
                    .llvm_ctx("address typed root metadata")?;
                builder
                    .build_store(field, entry)
                    .llvm_ctx("bind typed root metadata")?;
            }
        }
        self.bind_managed_roots(ctx, llvm, builder, table, u64::from(count))?;
        Ok(())
    }

    pub(super) fn bind_managed_roots(
        &self,
        ctx: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
        table: PointerValue<'ctx>,
        count: u64,
    ) -> CodegenResult<()> {
        let pointer = ctx.ptr_type(AddressSpace::default());
        let view_ty = self.managed_frame_type;
        let size = view_ty.get_field_type_at_index(1).unwrap().into_int_type();
        let view = if let Some(view) = self.managed_view.get() {
            view
        } else {
            let view = self.storage(ctx, view_ty.into(), "managed.frame")?;
            self.managed_view.set(Some(view));
            view
        };
        let (operation, _) = self.operation_storage(ctx)?;
        let child = self.child_storage(ctx)?.0;
        let entries: [BasicValueEnum<'ctx>; 4] = [
            table.into(),
            size.const_int(count, false).into(),
            child.into(),
            operation.into(),
        ];
        for (index, entry) in entries.into_iter().enumerate() {
            let field = builder
                .build_struct_gep(view_ty, view, index as u32, "managed.frame.field")
                .llvm_ctx("address canonical invocation roots")?;
            builder
                .build_store(field, entry)
                .llvm_ctx("bind canonical invocation roots")?;
        }
        let bind = get_or_declare_external(
            module,
            "hew_budget_bind_frame",
            ctx.void_type().fn_type(&[pointer.into(); 2], false),
        )?;
        builder
            .build_call(bind, &[self.state.into(), view.into()], "")
            .llvm_ctx("publish canonical invocation roots")?;
        Ok(())
    }

    fn operation_storage(
        &self,
        ctx: &'ctx Context,
    ) -> CodegenResult<(PointerValue<'ctx>, PointerValue<'ctx>)> {
        if let Some(operation) = self.managed_operation.get() {
            return Ok(operation);
        }
        let pointer = ctx.ptr_type(AddressSpace::default());
        let row = ctx.struct_type(&[pointer.into(); 2], false);
        let metadata = self.storage(ctx, row.into(), "managed.operation")?;
        let owner = self.storage(ctx, pointer.into(), "managed.operation.owner")?;
        let builder = ctx.create_builder();
        if let Some(end) = self.entry.get_terminator() {
            builder.position_before(&end);
        } else {
            builder.position_at_end(self.entry);
        }
        let layout = builder
            .build_struct_gep(row, metadata, 0, "managed.operation.layout")
            .llvm_ctx("address active operation descriptor")?;
        let slot = builder
            .build_struct_gep(row, metadata, 1, "managed.operation.slot")
            .llvm_ctx("address active operation owner")?;
        builder
            .build_store(layout, pointer.const_null())
            .llvm_ctx("initialize inactive operation")?;
        builder
            .build_store(slot, pointer.const_null())
            .llvm_ctx("initialize inactive operation slot")?;
        builder
            .build_store(owner, pointer.const_null())
            .llvm_ctx("initialize protocol owner")?;
        let result = (metadata, owner);
        self.managed_operation.set(Some(result));
        Ok(result)
    }

    pub(super) fn bind_operation(
        &self,
        ctx: &'ctx Context,
        builder: &Builder<'ctx>,
        layout: PointerValue<'ctx>,
        owner: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let (metadata, slot) = self.operation_storage(ctx)?;
        let pointer = ctx.ptr_type(AddressSpace::default());
        let row = ctx.struct_type(&[pointer.into(); 2], false);
        let field = builder
            .build_struct_gep(row, metadata, 0, "managed.operation.layout")
            .llvm_ctx("address active protocol descriptor")?;
        let address = builder
            .build_struct_gep(row, metadata, 1, "managed.operation.slot")
            .llvm_ctx("address active protocol slot")?;
        builder
            .build_store(slot, owner)
            .llvm_ctx("retain current protocol handle")?;
        // Publish this address after coro.begin, when coroutine splitting can
        // place the owner cell in its persistent frame. A pointer stored in
        // the ramp's aggregate initializer would retain the ramp stack slot.
        builder
            .build_store(address, slot)
            .llvm_ctx("bind persistent protocol handle slot")?;
        builder
            .build_store(field, layout)
            .llvm_ctx("publish typed protocol owner")?;
        Ok(())
    }

    pub(super) fn clear_operation(
        &self,
        ctx: &'ctx Context,
        builder: &Builder<'ctx>,
    ) -> CodegenResult<()> {
        let pointer = ctx.ptr_type(AddressSpace::default());
        self.bind_operation(ctx, builder, pointer.const_null(), pointer.const_null())
    }

    pub fn init_child(
        &self,
        ctx: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) -> CodegenResult<PointerValue<'ctx>> {
        self.init_child_with(ctx, module, builder, "hew_coro_state_init_child")
    }

    pub fn init_child_with(
        &self,
        ctx: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
        init_symbol: &str,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let pointer = ctx.ptr_type(AddressSpace::default());
        let storage = if let Some(storage) = self.child_invocation.get() {
            storage
        } else {
            let record = invocation_type(ctx);
            let storage = self.storage(ctx, record.into(), "call.child.invocation")?;
            self.child_invocation.set(Some(storage));
            storage
        };
        let init = get_or_declare_external(
            module,
            init_symbol,
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
        let builder = ctx.create_builder();
        if let Some(terminator) = self.entry.get_terminator() {
            builder.position_before(&terminator);
        } else {
            builder.position_at_end(self.entry);
        }
        builder
            .build_store(slots.0, pointer.const_null())
            .llvm_ctx("initialize child root view")?;
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

fn managed_frame_type<'ctx>(
    ctx: &'ctx Context,
    target_data: &TargetData,
) -> inkwell::types::StructType<'ctx> {
    let pointer = ctx.ptr_type(AddressSpace::default());
    let size = ctx.ptr_sized_int_type(target_data, None);
    ctx.struct_type(
        &[pointer.into(), size.into(), pointer.into(), pointer.into()],
        false,
    )
}

#[cfg(test)]
mod abi_tests {
    use super::super::actor::{c_mirror_mismatch, field_size};
    use super::*;
    use hew_cabi::coro::HewCoroInvocation;
    use std::mem::{align_of, offset_of, size_of};

    #[test]
    fn invocation_storage_matches_the_runtime_c_abi() {
        let fields = [
            (
                offset_of!(HewCoroInvocation, context),
                field_size(|record: &HewCoroInvocation| &record.context),
            ),
            (
                offset_of!(HewCoroInvocation, status),
                field_size(|record: &HewCoroInvocation| &record.status),
            ),
            (
                offset_of!(HewCoroInvocation, private_status),
                field_size(|record: &HewCoroInvocation| &record.private_status),
            ),
            (
                offset_of!(HewCoroInvocation, poll_suppression),
                field_size(|record: &HewCoroInvocation| &record.poll_suppression),
            ),
            (
                offset_of!(HewCoroInvocation, lexical_depth),
                field_size(|record: &HewCoroInvocation| &record.lexical_depth),
            ),
            (
                offset_of!(HewCoroInvocation, flags),
                field_size(|record: &HewCoroInvocation| &record.flags),
            ),
            (
                offset_of!(HewCoroInvocation, cleanup_fault),
                field_size(|record: &HewCoroInvocation| &record.cleanup_fault),
            ),
            (
                offset_of!(HewCoroInvocation, cleanup_abandon_io),
                field_size(|record: &HewCoroInvocation| &record.cleanup_abandon_io),
            ),
            (
                offset_of!(HewCoroInvocation, managed_frame),
                field_size(|record: &HewCoroInvocation| &record.managed_frame),
            ),
        ];
        let ctx = Context::create();
        let record = invocation_type(&ctx);
        let mismatch = |mirror| {
            c_mirror_mismatch(
                mirror,
                &fields,
                size_of::<HewCoroInvocation>(),
                align_of::<HewCoroInvocation>(),
            )
        };
        assert_eq!(mismatch(record), None);
        let mut reordered = record.get_field_types();
        reordered.swap(0, 1);
        assert!(mismatch(ctx.struct_type(&reordered, false)).is_some());
    }
}
