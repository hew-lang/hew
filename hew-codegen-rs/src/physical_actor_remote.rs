//! `RemotePid` send and ask: the message moves into the runtime, which encodes
//! it for a peer or delivers its fields to an actor on this node.

use super::*;
use hew_mir::physical::RemoteObservationKind;

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    fn remote_member(&self, actor: ActorId, message: u32) -> CodegenResult<&SemActorHandler> {
        self.module
            .actors
            .get(actor.0 as usize)
            .and_then(|actor| {
                actor
                    .handlers
                    .iter()
                    .find(|handler| handler.message_id == message && handler.codec.is_some())
            })
            .ok_or_else(|| CodegenError::FailClosed("remote call lacks its codec member".into()))
    }

    fn remote_dispatch(&self, actor: ActorId) -> CodegenResult<PointerValue<'ctx>> {
        self.llvm
            .get_function(&symbol(actor, "dispatch"))
            .map(|dispatch| dispatch.as_global_value().as_pointer_value())
            .ok_or_else(|| CodegenError::FailClosed("remote call lacks its codec key".into()))
    }

    /// Move the message into a stack wrapper, returning the wrapper and its
    /// size. The runtime takes the wrapper's fields and leaves its bytes.
    fn remote_request(
        &self,
        handler: &SemActorHandler,
        payload: &[StorageId],
    ) -> CodegenResult<(PointerValue<'ctx>, IntValue<'ctx>)> {
        let wrapper_ty = message_type(self.module, self.ctx, handler)?;
        let target = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let entry = self.ctx.create_builder();
        let block = self.value.get_first_basic_block().unwrap();
        if let Some(first) = block.get_first_instruction() {
            entry.position_before(&first);
        } else {
            entry.position_at_end(block);
        }
        let wrapper = entry
            .build_alloca(wrapper_ty, "remote.request")
            .llvm_ctx("allocate remote request")?;
        self.builder
            .build_store(wrapper, self.ctx.i8_type().const_int(1, false))
            .llvm_ctx("own remote request")?;
        if payload.len() != handler.params.len() {
            return Err(CodegenError::FailClosed(
                "remote request differs from its full handler layout".into(),
            ));
        }
        for (index, value) in payload.iter().enumerate() {
            let field = self
                .builder
                .build_struct_gep(
                    wrapper_ty,
                    wrapper,
                    index as u32 + 1,
                    "remote.request.field",
                )
                .llvm_ctx("address remote argument")?;
            let layout = self
                .module
                .target
                .layout(&handler.params[index])
                .ok_or_else(|| {
                    CodegenError::FailClosed("remote argument lacks target layout".into())
                })?;
            if layout.size != 0 {
                self.builder
                    .build_store(field, self.load(*value, "remote.argument")?)
                    .llvm_ctx("move remote argument")?;
            }
            self.clear_owned(*value)?;
        }
        Ok((
            wrapper,
            size_ty.const_int(target.get_abi_size(&wrapper_ty), false),
        ))
    }

    #[expect(
        clippy::too_many_arguments,
        reason = "one exact remote suspension contract"
    )]
    pub(in crate::physical) fn emit_remote_ask(
        &self,
        actor: ActorId,
        message: u32,
        target: StorageId,
        payload: &[StorageId],
        timeout: StorageId,
        result: StorageId,
        normal: &PhysicalEdge,
        cancel: &PhysicalEdge,
        unwind: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("remote ask requires a resumable invocation".into())
        })?;
        let handler = self.remote_member(actor, message)?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let target_data = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target_data, None);
        let i32_ty = self.ctx.i32_type();
        let (wrapper, size) = self.remote_request(handler, payload)?;
        let waker = call_value(
            &self.builder,
            get_or_declare_external(
                self.llvm,
                "hew_coro_state_waker",
                ptr.fn_type(&[ptr.into()], false),
            )?,
            &[frame.state.into()],
            "remote.waker",
        )?;
        let reply_layout = (handler.return_ty != ResolvedTy::Unit)
            .then(|| {
                self.module
                    .target
                    .layout(&handler.return_ty)
                    .ok_or_else(|| {
                        CodegenError::FailClosed("remote reply lacks its target layout".into())
                    })
            })
            .transpose()?;
        let start = get_or_declare_external(
            self.llvm,
            "hew_remote_call_new",
            ptr.fn_type(
                &[
                    ptr.into(),
                    ptr.into(),
                    i32_ty.into(),
                    ptr.into(),
                    size_ty.into(),
                    size_ty.into(),
                    self.ctx.i64_type().into(),
                    self.ctx.i64_type().into(),
                    ptr.into(),
                ],
                false,
            ),
        )?;
        let operation = call_value(
            &self.builder,
            start,
            &[
                self.slots[target.0 as usize].into(),
                self.remote_dispatch(actor)?.into(),
                i32_ty.const_int(u64::from(message), false).into(),
                wrapper.into(),
                size.into(),
                size_ty
                    .const_int(reply_layout.map_or(0, |layout| layout.size), false)
                    .into(),
                self.load(timeout, "remote.timeout")?.into(),
                self.request_deadline()?.into(),
                waker.into(),
            ],
            "remote.operation",
        )?
        .into_pointer_value();

        self.emit_remote_operation(
            operation,
            &handler.return_ty,
            result,
            normal,
            cancel,
            unwind,
        )
    }

    fn emit_remote_operation(
        &self,
        operation: PointerValue<'ctx>,
        reply_ty: &ResolvedTy,
        result: StorageId,
        normal: &PhysicalEdge,
        cancel: &PhysicalEdge,
        unwind: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("remote operation requires a resumable invocation".into())
        })?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let i32_ty = self.ctx.i32_type();
        let reply_layout = (*reply_ty != ResolvedTy::Unit)
            .then(|| {
                self.module.target.layout(reply_ty).ok_or_else(|| {
                    CodegenError::FailClosed("remote reply lacks target layout".into())
                })
            })
            .transpose()?;
        let poll = self.ctx.append_basic_block(self.value, "remote.poll");
        let inspect = self.ctx.append_basic_block(self.value, "remote.inspect");
        let pending = self.ctx.append_basic_block(self.value, "remote.pending");
        let completed = self.ctx.append_basic_block(self.value, "remote.completed");
        let cancelled = self.ctx.append_basic_block(self.value, "remote.cancelled");
        let destroyed = self.ctx.append_basic_block(self.value, "remote.destroyed");
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("poll remote reply")?;
        self.builder.position_at_end(poll);
        let cancelling = self.state_value("hew_coro_state_is_cancelled", frame.state)?;
        let cancelling = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                cancelling,
                i32_ty.const_zero(),
                "remote.cancel.requested",
            )
            .llvm_ctx("inspect remote cancellation")?;
        self.builder
            .build_conditional_branch(cancelling, cancelled, inspect)
            .llvm_ctx("select remote cancellation")?;
        self.builder.position_at_end(inspect);
        let status = self.state_value("hew_remote_call_poll", operation)?;
        let waiting = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                i32_ty.const_all_ones(),
                "remote.waiting",
            )
            .llvm_ctx("inspect remote readiness")?;
        self.builder
            .build_conditional_branch(waiting, pending, completed)
            .llvm_ctx("select remote readiness")?;
        self.builder.position_at_end(pending);
        frame.suspend(self.ctx, &self.builder, poll, destroyed)?;
        self.builder.position_at_end(destroyed);
        self.builder
            .build_unconditional_branch(cancelled)
            .llvm_ctx("abandon destroyed remote call")?;

        self.builder.position_at_end(completed);
        let reply = reply_layout
            .map(|layout| {
                let entry = self.ctx.create_builder();
                let block = self.value.get_first_basic_block().unwrap();
                if let Some(first) = block.get_first_instruction() {
                    entry.position_before(&first);
                } else {
                    entry.position_at_end(block);
                }
                entry
                    .build_alloca(llvm_type(self.ctx, &layout.repr)?, "remote.reply")
                    .llvm_ctx("allocate remote reply")
            })
            .transpose()?;
        let take = get_or_declare_external(
            self.llvm,
            "hew_remote_call_take",
            i32_ty.fn_type(&[ptr.into(), ptr.into()], false),
        )?;
        let status = call_value(
            &self.builder,
            take,
            &[operation.into(), reply.unwrap_or(ptr.const_null()).into()],
            "remote.outcome",
        )?
        .into_int_value();
        self.emit_ask_result(result, status, reply, reply_ty)?;
        // Both exits converge on one release of the call.
        let mut join = self.exit_join("remote.release");
        self.leave(&mut join, suspend::EXIT_NORMAL)?;
        self.builder.position_at_end(cancelled);
        self.initialize_cancellation_fault()?;
        self.leave(&mut join, suspend::EXIT_CANCEL)?;
        let selected = self.enter_join(&join)?;
        self.release_handle(release::Handle::RemoteCall, operation)?;
        // Every remote failure is a typed `Err`; no fault reaches `unwind`.
        let _ = unwind;
        let ([normal_block], cancel_block) = self.dispatch_exits(
            selected,
            [(suspend::EXIT_NORMAL, "remote.normal")],
            "remote.cancel",
        )?;
        self.builder.position_at_end(normal_block);
        self.emit_result_edge(Some(result), normal)?;
        self.builder.position_at_end(cancel_block);
        self.emit_edge(cancel)
    }

    /// The node's exact-location submission status, which HIR folds into
    /// `Result<(), SendError>`.
    #[expect(
        clippy::too_many_arguments,
        reason = "one typed service acquisition and its continuations"
    )]
    pub(in crate::physical) fn emit_service_acquire(
        &self,
        actor: ActorId,
        export: bool,
        args: &[ArgumentTransfer],
        result: StorageId,
        normal: &PhysicalEdge,
        cancel: &PhysicalEdge,
        unwind: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("service acquisition requires an invocation".into())
        })?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let target = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let descriptor =
            self.module.actors.get(actor.0 as usize).ok_or_else(|| {
                CodegenError::FailClosed("service acquisition lacks actor".into())
            })?;
        let name = format!("__hew_service_interface_{}", actor.0);
        let interface = if let Some(global) = self.llvm.get_global(&name) {
            global
        } else {
            let handlers = descriptor
                .handlers
                .iter()
                .filter(|handler| handler.codec.is_some())
                .map(|handler| {
                    self.builder
                        .build_global_string_ptr(&handler.wire_identity, "service.handler")
                        .llvm_ctx("encode service handler identity")
                        .map(|value| value.as_pointer_value())
                })
                .collect::<CodegenResult<Vec<_>>>()?;
            let initializer = ptr.const_array(&handlers);
            let global = self.llvm.add_global(initializer.get_type(), None, &name);
            global.set_initializer(&initializer);
            global.set_constant(true);
            global.set_linkage(Linkage::Private);
            global
        };
        let source = |index: usize| {
            args.get(index)
                .and_then(|argument| match argument {
                    ArgumentTransfer::Borrow(id) => Some(*id),
                    _ => None,
                })
                .ok_or_else(|| {
                    CodegenError::FailClosed("service acquisition lacks borrowed input".into())
                })
        };
        let waker = call_value(
            &self.builder,
            get_or_declare_external(
                self.llvm,
                "hew_coro_state_waker",
                ptr.fn_type(&[ptr.into()], false),
            )?,
            &[frame.state.into()],
            "service.waker",
        )?;
        let start = get_or_declare_external(
            self.llvm,
            "hew_service_acquire_new",
            ptr.fn_type(
                &[
                    ptr.into(),
                    self.ctx.i64_type().into(),
                    ptr.into(),
                    size_ty.into(),
                    self.ctx.bool_type().into(),
                    self.ctx.i64_type().into(),
                    ptr.into(),
                ],
                false,
            ),
        )?;
        let operation = call_value(
            &self.builder,
            start,
            &[
                self.load(source(0)?, "service.peer")?.into(),
                if export {
                    self.load(source(1)?, "service.target")?.into()
                } else {
                    self.ctx.i64_type().const_zero().into()
                },
                interface.as_pointer_value().into(),
                size_ty
                    .const_int(
                        descriptor
                            .handlers
                            .iter()
                            .filter(|handler| handler.codec.is_some())
                            .count() as u64,
                        false,
                    )
                    .into(),
                self.ctx
                    .bool_type()
                    .const_int(u64::from(export), false)
                    .into(),
                self.request_deadline()?.into(),
                waker.into(),
            ],
            "service.operation",
        )?
        .into_pointer_value();
        let ResolvedTy::Named { args, .. } = &self.storage(result)?.ty else {
            return Err(CodegenError::FailClosed(
                "service acquisition lacks Result".into(),
            ));
        };
        let reply_ty = args.first().ok_or_else(|| {
            CodegenError::FailClosed("service acquisition lacks success type".into())
        })?;
        self.emit_remote_operation(operation, reply_ty, result, normal, cancel, unwind)
    }

    pub(super) fn emit_remote_send(
        &self,
        actor: ActorId,
        message: u32,
        transfers: &[ArgumentTransfer],
        result: Option<StorageId>,
        normal: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let [ArgumentTransfer::Borrow(target), ArgumentTransfer::Move(payload)] = transfers else {
            return Err(CodegenError::FailClosed(
                "remote send borrows its pid and moves its message".into(),
            ));
        };
        let result =
            result.ok_or_else(|| CodegenError::FailClosed("remote send has no status".into()))?;
        let handler = self.remote_member(actor, message)?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let size_ty = self
            .ctx
            .ptr_sized_int_type(&TargetData::create(&self.module.target.data_layout), None);
        let i32_ty = self.ctx.i32_type();
        let (wrapper, size) = self.remote_request(handler, std::slice::from_ref(payload))?;
        let send = get_or_declare_external(
            self.llvm,
            "hew_node_api_send_location",
            i32_ty.fn_type(
                &[
                    ptr.into(),
                    ptr.into(),
                    i32_ty.into(),
                    ptr.into(),
                    size_ty.into(),
                ],
                false,
            ),
        )?;
        let status = call_value(
            &self.builder,
            send,
            &[
                self.slots[target.0 as usize].into(),
                self.remote_dispatch(actor)?.into(),
                i32_ty.const_int(u64::from(message), false).into(),
                wrapper.into(),
                size.into(),
            ],
            "remote.send.status",
        )?;
        self.store(result, status)?;
        self.emit_result_edge(Some(result), normal)
    }

    /// `link_remote(pid, policy)` and `monitor(pid)` through the node's
    /// exact-location setup; the runtime status selects the typed result.
    pub(super) fn emit_remote_observation(
        &self,
        kind: RemoteObservationKind,
        params: &[ResolvedTy],
        transfers: &[ArgumentTransfer],
        result: Option<StorageId>,
        normal: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let i32_ty = self.ctx.i32_type();
        let (status, monitor_id) = match (kind, transfers) {
            (
                RemoteObservationKind::Link,
                [ArgumentTransfer::Borrow(target), ArgumentTransfer::Move(policy)],
            ) => {
                // PartitionPolicy is tagged by declaration order, as the node
                // expects: `CrashLinked` crashes the local linked actor.
                let glue = self
                    .module
                    .variant_glue
                    .iter()
                    .find(|glue| glue.ty == params[1])
                    .ok_or_else(|| {
                        CodegenError::FailClosed("link policy lacks its variant recipe".into())
                    })?;
                let (tag, _, _) = self.load_variant_tag(*policy, glue.id)?;
                let tag = self
                    .builder
                    .build_int_z_extend_or_bit_cast(tag, self.ctx.i64_type(), "link.policy")
                    .llvm_ctx("widen link policy")?;
                self.clear_owned(*policy)?;
                let link = get_or_declare_external(
                    self.llvm,
                    "hew_node_link_remote_location",
                    i32_ty.fn_type(&[ptr.into(), self.ctx.i64_type().into()], false),
                )?;
                let status = call_value(
                    &self.builder,
                    link,
                    &[self.slots[target.0 as usize].into(), tag.into()],
                    "link.remote.status",
                )?;
                (status.into_int_value(), None)
            }
            (RemoteObservationKind::Monitor, [ArgumentTransfer::Borrow(target)]) => {
                let entry = self.ctx.create_builder();
                let block = self.value.get_first_basic_block().unwrap();
                if let Some(first) = block.get_first_instruction() {
                    entry.position_before(&first);
                } else {
                    entry.position_at_end(block);
                }
                let id = entry
                    .build_alloca(self.ctx.i64_type(), "monitor.remote.id")
                    .llvm_ctx("allocate remote monitor identity")?;
                let monitor = get_or_declare_external(
                    self.llvm,
                    "hew_node_monitor_location",
                    i32_ty.fn_type(&[ptr.into(), ptr.into()], false),
                )?;
                let status = call_value(
                    &self.builder,
                    monitor,
                    &[self.slots[target.0 as usize].into(), id.into()],
                    "monitor.remote.status",
                )?;
                (status.into_int_value(), Some(id))
            }
            _ => {
                return Err(CodegenError::FailClosed(
                    "remote observation borrows its pid and consumes its policy".into(),
                ))
            }
        };
        self.emit_observation_result(result, status, monitor_id, normal)
    }
}
