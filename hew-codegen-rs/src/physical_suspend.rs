//! Execute explicit physical suspension and invoke resumable private callables.

use super::*;

pub(super) fn call_value<'ctx>(
    builder: &Builder<'ctx>,
    function: FunctionValue<'ctx>,
    args: &[BasicMetadataValueEnum<'ctx>],
    name: &str,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    builder
        .build_call(function, args, name)
        .llvm_ctx("call coroutine runtime")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("coroutine runtime returned no value".into()))
}

impl<'ctx> ModuleEmitter<'ctx, '_> {
    /// One root/task-thread driver also serves a synchronous hosting boundary.
    /// Parameters are packed only by this adapter, while the body keeps its
    /// exact physical parameter carriers.
    pub(super) fn emit_sync_wrapper(&self, callable: &PhysicalCallable) -> CodegenResult<()> {
        let wrapper = self.functions[&callable.id];
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(wrapper, "invoke"));
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let params: Vec<_> = wrapper
            .get_params()
            .into_iter()
            .take(callable.params.len())
            .collect();
        let environment = if params.is_empty() {
            pointer.const_null()
        } else {
            let types: Vec<_> = params.iter().map(|param| param.get_type()).collect();
            let ty = self.ctx.struct_type(&types, false);
            let env = builder
                .build_alloca(ty, "invoke.arguments")
                .llvm_ctx("allocate invocation arguments")?;
            for (index, param) in params.iter().enumerate() {
                let slot = builder
                    .build_struct_gep(ty, env, index as u32, "invoke.argument")
                    .llvm_ctx("address invocation argument")?;
                builder
                    .build_store(slot, *param)
                    .llvm_ctx("pack invocation argument")?;
            }
            env
        };
        let index = callable.params.len() as u32;
        let result = if callable.return_layout.is_some() {
            wrapper.get_nth_param(index).unwrap().into_pointer_value()
        } else {
            pointer.const_null()
        };
        let fault = wrapper
            .get_nth_param(index + u32::from(callable.return_layout.is_some()))
            .unwrap()
            .into_pointer_value();
        let status = self.emit_root_invocation(&builder, callable, environment, result, fault)?;
        builder
            .build_return(Some(&status))
            .llvm_ctx("return hosted invocation status")?;
        Ok(())
    }

    /// Shared entry hook: the process adapter owns scheduler lifecycle around
    /// this call. A parameterless process root passes a null environment.
    pub(super) fn emit_root_invocation(
        &self,
        builder: &Builder<'ctx>,
        callable: &PhysicalCallable,
        environment: PointerValue<'ctx>,
        result: PointerValue<'ctx>,
        fault: PointerValue<'ctx>,
    ) -> CodegenResult<IntValue<'ctx>> {
        let start = self.emit_root_start(callable)?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let run = coro::external(
            &self.llvm,
            "hew_coro_run_root",
            self.ctx.i32_type().fn_type(&[pointer.into(); 4], false),
        )?;
        Ok(call_value(
            builder,
            run,
            &[
                start.as_global_value().as_pointer_value().into(),
                environment.into(),
                result.into(),
                fault.into(),
            ],
            "root.status",
        )?
        .into_int_value())
    }

    fn emit_root_start(&self, callable: &PhysicalCallable) -> CodegenResult<FunctionValue<'ctx>> {
        let symbol = format!("{}$root_start", emitted_symbol(self.module, callable));
        if let Some(function) = self.llvm.get_function(&symbol) {
            return Ok(function);
        }
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let start = self.llvm.add_function(
            &symbol,
            pointer.fn_type(&[pointer.into(); 4], false),
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(start, "start"));
        let environment = start.get_nth_param(0).unwrap().into_pointer_value();
        let result = start.get_nth_param(1).unwrap();
        let fault = start.get_nth_param(2).unwrap();
        let state = start.get_nth_param(3).unwrap();
        let fields = callable
            .params
            .iter()
            .map(|param| match param.carrier {
                ParamCarrier::Direct => llvm_type(self.ctx, &param.layout.repr),
                ParamCarrier::Indirect => Ok(pointer.into()),
            })
            .collect::<CodegenResult<Vec<_>>>()?;
        let environment_ty = self.ctx.struct_type(&fields, false);
        let mut args = Vec::new();
        for (index, ty) in fields.iter().enumerate() {
            let slot = builder
                .build_struct_gep(environment_ty, environment, index as u32, "argument")
                .llvm_ctx("address root argument")?;
            args.push(
                builder
                    .build_load(*ty, slot, "argument.value")
                    .llvm_ctx("load root argument")?
                    .into(),
            );
        }
        if callable.return_layout.is_some() {
            args.push(result.into());
        }
        args.push(fault.into());
        if callable.is_resumable {
            args.push(state.into());
            let frame = call_value(&builder, self.ramps[&callable.id], &args, "root.frame")?;
            builder
                .build_return(Some(&frame))
                .llvm_ctx("return root continuation")?;
        } else {
            let status = call_value(&builder, self.functions[&callable.id], &args, "root.status")?;
            let finish = coro::external(
                &self.llvm,
                "hew_coro_state_finish",
                self.ctx
                    .i32_type()
                    .fn_type(&[pointer.into(), self.ctx.i32_type().into()], false),
            )?;
            builder
                .build_call(finish, &[state.into(), status.into()], "")
                .llvm_ctx("publish synchronous root outcome")?;
            builder
                .build_return(Some(&pointer.const_null()))
                .llvm_ctx("finish synchronous root")?;
        }
        Ok(start)
    }
}

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    pub(super) fn emit_finish(&self, status: IntValue<'ctx>) -> CodegenResult<()> {
        if let Some(frame) = &self.frame {
            let pointer = self.ctx.ptr_type(AddressSpace::default());
            let finish = coro::external(
                self.llvm,
                "hew_coro_state_finish",
                self.ctx
                    .i32_type()
                    .fn_type(&[pointer.into(), self.ctx.i32_type().into()], false),
            )?;
            self.builder
                .build_call(finish, &[frame.state.into(), status.into()], "")
                .llvm_ctx("publish checked coroutine outcome")?;
            self.builder
                .build_unconditional_branch(frame.finish)
                .llvm_ctx("finish checked coroutine")?;
        } else {
            self.builder
                .build_return(Some(&status))
                .llvm_ctx("return checked status")?;
        }
        Ok(())
    }

    pub(super) fn state_value(
        &self,
        name: &str,
        state: PointerValue<'ctx>,
    ) -> CodegenResult<IntValue<'ctx>> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let function = coro::external(
            self.llvm,
            name,
            self.ctx.i32_type().fn_type(&[pointer.into()], false),
        )?;
        Ok(call_value(&self.builder, function, &[state.into()], "state.status")?.into_int_value())
    }

    pub(super) fn free_handle(&self, name: &str, handle: PointerValue<'ctx>) -> CodegenResult<()> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let free = coro::external(
            self.llvm,
            name,
            self.ctx.void_type().fn_type(&[pointer.into()], false),
        )?;
        self.builder
            .build_call(free, &[handle.into()], "")
            .llvm_ctx("release coroutine operation")?;
        Ok(())
    }

    pub(super) fn emit_sleep(
        &self,
        duration: StorageId,
        normal: &PhysicalEdge,
        cancel: &PhysicalEdge,
        unwind: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let frame = self
            .frame
            .as_ref()
            .ok_or_else(|| CodegenError::FailClosed("sleep requires a resumable body".into()))?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let waker_fn = coro::external(
            self.llvm,
            "hew_coro_state_waker",
            pointer.fn_type(&[pointer.into()], false),
        )?;
        let waker = call_value(
            &self.builder,
            waker_fn,
            &[frame.state.into()],
            "sleep.waker",
        )?;
        let start = coro::external(
            self.llvm,
            "hew_coro_sleep_new",
            pointer.fn_type(&[self.ctx.i64_type().into(), pointer.into()], false),
        )?;
        let duration = self.load(duration, "sleep.duration")?;
        let operation = call_value(
            &self.builder,
            start,
            &[duration.into(), waker.into()],
            "sleep.operation",
        )?
        .into_pointer_value();
        let poll = self.ctx.append_basic_block(self.value, "sleep.poll");
        let poll_status = self.ctx.append_basic_block(self.value, "sleep.poll.status");
        let waiting = self.ctx.append_basic_block(self.value, "sleep.pending");
        let cancelled = self.ctx.append_basic_block(self.value, "sleep.cancelled");
        let destroyed = self.ctx.append_basic_block(self.value, "sleep.destroyed");
        let completed = self.ctx.append_basic_block(self.value, "sleep.completed");
        let failed = self.ctx.append_basic_block(self.value, "sleep.failed");
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("poll sleep")?;
        self.builder.position_at_end(poll);
        let cancellation = self.state_value("hew_coro_state_is_cancelled", frame.state)?;
        let cancellation = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                cancellation,
                self.ctx.i32_type().const_zero(),
                "sleep.cancel.requested",
            )
            .llvm_ctx("test sleep cancellation")?;
        self.builder
            .build_conditional_branch(cancellation, cancelled, poll_status)
            .llvm_ctx("select sleep cancellation")?;
        self.builder.position_at_end(poll_status);
        let status = self.state_value("hew_coro_sleep_status", operation)?;
        self.builder
            .build_switch(
                status,
                failed,
                &[
                    (self.ctx.i32_type().const_zero(), waiting),
                    (self.ctx.i32_type().const_int(1, false), completed),
                ],
            )
            .llvm_ctx("select sleep outcome")?;
        self.builder.position_at_end(waiting);
        frame.suspend(self.ctx, self.llvm, &self.builder, poll, destroyed, false)?;
        self.builder.position_at_end(destroyed);
        self.builder
            .build_store(frame.destroying, self.ctx.bool_type().const_int(1, false))
            .llvm_ctx("mark destroyed sleep frame")?;
        self.builder
            .build_unconditional_branch(cancelled)
            .llvm_ctx("run sleep cancellation cleanup")?;
        self.builder.position_at_end(completed);
        self.free_handle("hew_coro_sleep_free", operation)?;
        self.emit_edge(normal)?;
        for (block, edge, code) in [
            (cancelled, cancel, hew_runtime::fault::HEW_FAULT_CANCELLED),
            (failed, unwind, HEW_TRAP_USER_PANIC),
        ] {
            self.builder.position_at_end(block);
            self.free_handle("hew_coro_sleep_free", operation)?;
            self.initialize_active_fault(code)?;
            self.emit_edge(edge)?;
        }
        Ok(())
    }

    pub(super) fn emit_resumable_call(
        &self,
        callee: CallableId,
        args: &[BasicMetadataValueEnum<'ctx>],
        moved: &[StorageId],
    ) -> CodegenResult<IntValue<'ctx>> {
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("resumable call requires a resumable caller".into())
        })?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let child_fn = coro::external(
            self.llvm,
            "hew_coro_state_child",
            pointer.fn_type(&[pointer.into()], false),
        )?;
        let child = call_value(
            &self.builder,
            child_fn,
            &[frame.state.into()],
            "call.child.state",
        )?
        .into_pointer_value();
        let mut args = args.to_vec();
        args.push(child.into());
        let child_frame = call_value(
            &self.builder,
            self.ramps[&callee],
            &args,
            "call.child.frame",
        )?
        .into_pointer_value();
        for source in moved {
            self.clear_owned(*source)?;
        }
        let status = await_child(
            self.ctx,
            self.llvm,
            &self.builder,
            self.value,
            frame,
            child,
            child_frame,
        )?;
        self.builder
            .build_store(self.active_status, status)
            .llvm_ctx("retain child status")?;
        Ok(status)
    }
}

/// Await a child frame while preserving the caller's own continuation.
/// Both private calls and erased callable adapters use this one frame protocol.
pub(super) fn await_child<'ctx>(
    ctx: &'ctx Context,
    llvm: &Module<'ctx>,
    builder: &Builder<'ctx>,
    function: FunctionValue<'ctx>,
    frame: &coro::Frame<'ctx>,
    child: PointerValue<'ctx>,
    child_frame: PointerValue<'ctx>,
) -> CodegenResult<IntValue<'ctx>> {
    let pointer = ctx.ptr_type(AddressSpace::default());
    let state_value = |name: &str, state: PointerValue<'ctx>| -> CodegenResult<IntValue<'ctx>> {
        let function =
            coro::external(llvm, name, ctx.i32_type().fn_type(&[pointer.into()], false))?;
        Ok(call_value(builder, function, &[state.into()], "child.status")?.into_int_value())
    };
    let free_handle = |name: &str, handle: PointerValue<'ctx>| -> CodegenResult<()> {
        let function = coro::external(
            llvm,
            name,
            ctx.void_type().fn_type(&[pointer.into()], false),
        )?;
        builder
            .build_call(function, &[handle.into()], "")
            .llvm_ctx("release child frame state")?;
        Ok(())
    };
    let poll = ctx.append_basic_block(function, "call.child.poll");
    let wait = ctx.append_basic_block(function, "call.child.wait");
    let resume = ctx.append_basic_block(function, "call.child.resume");
    let destroy = ctx.append_basic_block(function, "call.child.destroy");
    let done = ctx.append_basic_block(function, "call.child.done");
    let outcome = ctx.append_basic_block(function, "call.child.outcome");
    builder
        .build_unconditional_branch(poll)
        .llvm_ctx("poll child call")?;
    builder.position_at_end(poll);
    let status = state_value("hew_coro_state_status", child)?;
    let pending = builder
        .build_int_compare(
            IntPredicate::EQ,
            status,
            ctx.i32_type().const_zero(),
            "call.child.pending",
        )
        .llvm_ctx("test child call completion")?;
    builder
        .build_conditional_branch(pending, wait, done)
        .llvm_ctx("select child call completion")?;
    builder.position_at_end(wait);
    frame.suspend(ctx, llvm, builder, resume, destroy, false)?;
    builder.position_at_end(resume);
    free_handle("hew_cont_resume", child_frame)?;
    builder
        .build_unconditional_branch(poll)
        .llvm_ctx("poll resumed child")?;
    builder.position_at_end(destroy);
    builder
        .build_store(frame.destroying, ctx.bool_type().const_int(1, false))
        .llvm_ctx("mark destroyed caller")?;
    free_handle("hew_cont_destroy", child_frame)?;
    builder
        .build_unconditional_branch(outcome)
        .llvm_ctx("finish cancelled child")?;
    builder.position_at_end(done);
    free_handle("hew_cont_destroy", child_frame)?;
    builder
        .build_unconditional_branch(outcome)
        .llvm_ctx("finish completed child")?;
    builder.position_at_end(outcome);
    let status = state_value("hew_coro_state_private_status", child)?;
    free_handle("hew_coro_state_free", child)?;
    Ok(status)
}
