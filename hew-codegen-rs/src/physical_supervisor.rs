//! Native realization of the declared supervisor boundaries.
//!
//! A supervisor's construction arguments become one config allocation the
//! supervisor owns for its lifetime. Each declared child gets an adapter of the
//! fixed native spawn ABI: it reads the config, calls the child's verified
//! spawn callable — which re-runs the declared init arguments, `init()` and
//! `#[on(start)]` — and hands the runtime the resulting handle. The runtime
//! calls that same adapter for the initial spawn and for every restart, so an
//! incarnation is always reconstructed from the declaration and the config and
//! never from a captured state template.
//!
//! A supervised reference is a canonical runtime role token: every
//! use re-resolves the current incarnation through the supervisor.

use super::suspend::call_value;
use super::*;
use hew_mir::physical::{ParamCarrier, StorageId};
use hew_mir::physical::{
    SemRestartPolicy, SemRestartStrategy, SemSupervisedRole, SemSupervisor, SupervisorId,
};

fn symbol(supervisor: &SemSupervisor, suffix: &str) -> String {
    format!("__hew_supervisor_{}_{suffix}", supervisor.id.0)
}

fn strategy_code(strategy: SemRestartStrategy) -> u64 {
    use hew_runtime::supervisor as rt;
    let code = match strategy {
        SemRestartStrategy::OneForOne => rt::STRATEGY_ONE_FOR_ONE,
        SemRestartStrategy::OneForAll => rt::STRATEGY_ONE_FOR_ALL,
        SemRestartStrategy::RestForOne => rt::STRATEGY_REST_FOR_ONE,
        SemRestartStrategy::SimpleOneForOne => rt::STRATEGY_SIMPLE_ONE_FOR_ONE,
    };
    code as u64
}

fn restart_code(policy: SemRestartPolicy) -> u64 {
    use hew_runtime::supervisor as rt;
    let code = match policy {
        SemRestartPolicy::Permanent => rt::RESTART_PERMANENT,
        SemRestartPolicy::Transient => rt::RESTART_TRANSIENT,
        SemRestartPolicy::Temporary => rt::RESTART_TEMPORARY,
    };
    code as u64
}

fn role_kind(role: &SemSupervisedRole) -> u64 {
    let kind = match role {
        SemSupervisedRole::Actor(_) => hew_runtime::supervisor::ROLE_KIND_ACTOR,
        SemSupervisedRole::Supervisor(_) => hew_runtime::supervisor::ROLE_KIND_SUPERVISOR,
    };
    kind as u64
}

/// `HewNativeChildSpec`: restart policy, role kind, spawn adapter, name, deadline, startup rank and actor dispatch.
fn native_child_spec_type(ctx: &Context) -> inkwell::types::StructType<'_> {
    let ptr = ctx.ptr_type(AddressSpace::default());
    ctx.struct_type(
        &[
            ctx.i32_type().into(),
            ctx.i32_type().into(),
            ptr.into(),
            ptr.into(),
            ctx.i64_type().into(),
            ctx.i32_type().into(),
            ptr.into(),
            ctx.i64_type().into(),
            ctx.i64_type().into(),
        ],
        false,
    )
}

/// `HewNativeChildSpec` field holding the child's initial restart delay; the
/// maximum follows it.
const CHILD_BACKOFF_FIELD: u32 = 7;

/// The config allocation's LLVM shape, in declaration order.
fn config_type<'ctx>(
    module: &PhysicalModule,
    ctx: &'ctx Context,
    supervisor: &SemSupervisor,
) -> CodegenResult<inkwell::types::StructType<'ctx>> {
    let fields = supervisor
        .config
        .iter()
        .map(|ty| {
            llvm_type(
                ctx,
                &module
                    .target
                    .layout(ty)
                    .ok_or_else(|| {
                        CodegenError::FailClosed(
                            "supervisor config field lacks its target layout".into(),
                        )
                    })?
                    .repr,
            )
        })
        .collect::<CodegenResult<Vec<_>>>()?;
    Ok(ctx.struct_type(&fields, false))
}

impl<'ctx> ModuleEmitter<'ctx, '_> {
    pub(super) fn emit_supervisor_descriptors(&self) -> CodegenResult<()> {
        for supervisor in &self.module.supervisors {
            self.emit_supervisor_config_drop(supervisor)?;
            for child in 0..supervisor.children.len() {
                self.emit_supervisor_child_spawn(supervisor, child)?;
            }
            self.emit_supervisor_children(supervisor)?;
        }
        Ok(())
    }

    /// The declared children in construction order: restart policy, the
    /// adapter that produces each incarnation and the declared name.
    fn emit_supervisor_children(&self, supervisor: &SemSupervisor) -> CodegenResult<()> {
        let entry_ty = native_child_spec_type(self.ctx);
        let mut entries = Vec::new();
        for (index, child) in supervisor.children.iter().enumerate() {
            let spawn = self
                .llvm
                .get_function(&symbol(supervisor, &format!("child_{index}_spawn")))
                .ok_or_else(|| {
                    CodegenError::FailClosed("declared child lacks its spawn adapter".into())
                })?;
            let name = c_string_literal(
                self.ctx,
                &self.llvm,
                &child.name,
                &symbol(supervisor, &format!("child_{index}_name")),
            );
            let dispatch = match &child.role {
                SemSupervisedRole::Actor(actor) => self
                    .llvm
                    .get_function(&super::actor::symbol(*actor, "dispatch"))
                    .ok_or_else(|| {
                        CodegenError::FailClosed(
                            "declared actor child lacks its dispatch function".into(),
                        )
                    })?
                    .as_global_value()
                    .as_pointer_value(),
                SemSupervisedRole::Supervisor(_) => {
                    self.ctx.ptr_type(AddressSpace::default()).const_null()
                }
            };
            entries.push(
                entry_ty.const_named_struct(&[
                    self.ctx
                        .i32_type()
                        .const_int(restart_code(child.restart), false)
                        .into(),
                    self.ctx
                        .i32_type()
                        .const_int(role_kind(&child.role), false)
                        .into(),
                    spawn.as_global_value().as_pointer_value().into(),
                    name.into(),
                    self.ctx
                        .i64_type()
                        .const_int(
                            match child.stop_deadline {
                                hew_mir::physical::SemStopDeadline::DefaultActor => {
                                    hew_runtime::actor::DEFAULT_STOP_BUDGET_NS as u64
                                }
                                hew_mir::physical::SemStopDeadline::Literal(ns) => ns as u64,
                                hew_mir::physical::SemStopDeadline::Config(_) => 0,
                            },
                            true,
                        )
                        .into(),
                    self.ctx
                        .i32_type()
                        .const_int(
                            supervisor
                                .startup_order
                                .iter()
                                .position(|child| *child == index)
                                .ok_or_else(|| {
                                    CodegenError::FailClosed(
                                        "child missing from startup order".into(),
                                    )
                                })? as u64,
                            false,
                        )
                        .into(),
                    dispatch.into(),
                    self.ctx.i64_type().const_zero().into(),
                    self.ctx.i64_type().const_zero().into(),
                ]),
            );
        }
        let table = self.llvm.add_global(
            entry_ty.array_type(u32::try_from(entries.len()).map_err(|_| {
                CodegenError::FailClosed("supervisor child count exceeds u32".into())
            })?),
            None,
            &symbol(supervisor, "children"),
        );
        table.set_linkage(Linkage::Internal);
        table.set_constant(true);
        table.set_initializer(&entry_ty.const_array(&entries));
        Ok(())
    }

    /// Release what the config's fields own, once, at supervisor teardown.
    /// Emitted only when a config field actually owns something.
    fn emit_supervisor_config_drop(&self, supervisor: &SemSupervisor) -> CodegenResult<()> {
        let owned: Vec<_> = supervisor
            .config
            .iter()
            .enumerate()
            .filter_map(|(index, ty)| {
                self.module
                    .actor_recipes
                    .get(ty)
                    .and_then(|recipe| recipe.destroy)
                    .map(|action| (index, ty.clone(), action))
            })
            .collect();
        if owned.is_empty() {
            return Ok(());
        }
        let config_ty = config_type(self.module, self.ctx, supervisor)?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let function = self.llvm.add_function(
            &symbol(supervisor, "config_drop"),
            self.ctx.void_type().fn_type(&[ptr.into()], false),
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(function, "entry"));
        let config = function.get_first_param().unwrap().into_pointer_value();
        let emitter = ValueEmitter {
            module: self.module,
            target_data: self.target_data,
            ctx: self.ctx,
            llvm: &self.llvm,
            builder: &builder,
            value: function,
            fault_sink: None,
        };
        for (index, ty, action) in owned {
            let layout = self.module.target.layout(&ty).ok_or_else(|| {
                CodegenError::FailClosed("supervisor config field lacks its target layout".into())
            })?;
            let index = u32::try_from(index)
                .map_err(|_| CodegenError::FailClosed("config field exceeds u32".into()))?;
            let field = builder
                .build_struct_gep(config_ty, config, index, "config.field")
                .llvm_ctx("address owned config field")?;
            let value = builder
                .build_load(llvm_type(self.ctx, &layout.repr)?, field, "config.owner")
                .llvm_ctx("load owned config field")?;
            emitter.destroy_loaded_value(value, layout, action)?;
        }
        // The supervisor frees the buffer itself once teardown has run this
        // callback; releasing it here would free it twice.
        builder
            .build_return(None)
            .llvm_ctx("finish supervisor config release")?;
        Ok(())
    }

    /// Start one declared incarnation using its checked callable ABI. The runtime
    /// owns the output, fault and completion state until the returned frame ends.
    fn emit_supervisor_child_spawn(
        &self,
        supervisor: &SemSupervisor,
        child: usize,
    ) -> CodegenResult<()> {
        let spawn = callable(self.module, supervisor.children[child].spawn)?;
        let config_ty = config_type(self.module, self.ctx, supervisor)?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let target = self.target_data;
        let word = self.ctx.ptr_sized_int_type(target, None);
        let function = self.llvm.add_function(
            &symbol(supervisor, &format!("child_{child}_spawn")),
            ptr.fn_type(
                &[word.into(), ptr.into(), ptr.into(), ptr.into(), ptr.into()],
                false,
            ),
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(function, "entry"));
        let owner = function.get_first_param().unwrap().into_int_value();
        let config = function.get_nth_param(1).unwrap().into_pointer_value();
        let handle = function.get_nth_param(2).unwrap().into_pointer_value();
        let fault = function.get_nth_param(3).unwrap().into_pointer_value();
        let state = function.get_nth_param(4).unwrap().into_pointer_value();
        if spawn.return_layout.is_none() {
            return Err(CodegenError::FailClosed(
                "child spawn callable produces no handle".into(),
            ));
        }
        if spawn.params.len() != supervisor.config.len() + 1 {
            return Err(CodegenError::FailClosed(
                "child spawn callable does not take the supervisor config".into(),
            ));
        }
        let mut arguments: Vec<BasicMetadataValueEnum<'ctx>> = vec![owner.into()];
        for (index, parameter) in spawn.params.iter().skip(1).enumerate() {
            let index = u32::try_from(index)
                .map_err(|_| CodegenError::FailClosed("config field exceeds u32".into()))?;
            let field = builder
                .build_struct_gep(config_ty, config, index, "spawn.config.field")
                .llvm_ctx("address config argument")?;
            arguments.push(match parameter.carrier {
                ParamCarrier::Indirect => field.into(),
                ParamCarrier::Direct => builder
                    .build_load(
                        llvm_type(self.ctx, &parameter.layout.repr)?,
                        field,
                        "spawn.config.value",
                    )
                    .llvm_ctx("read config argument")?
                    .into(),
            });
        }
        arguments.push(handle.into());
        arguments.push(fault.into());
        if spawn.is_resumable {
            arguments.push(state.into());
            let frame = call_value(&builder, self.ramps[&spawn.id], &arguments, "spawn.frame")?;
            builder
                .build_return(Some(&frame))
                .llvm_ctx("return declared child continuation")?;
        } else {
            let status = call_value(
                &builder,
                self.functions[&spawn.id],
                &arguments,
                "spawn.status",
            )?;
            let finish = get_or_declare_external(
                &self.llvm,
                "hew_coro_state_finish",
                self.ctx
                    .i32_type()
                    .fn_type(&[ptr.into(), self.ctx.i32_type().into()], false),
            )?;
            builder
                .build_call(finish, &[state.into(), status.into()], "")
                .llvm_ctx("publish synchronous child outcome")?;
            builder
                .build_return(Some(&ptr.const_null()))
                .llvm_ctx("finish synchronous child construction")?;
        }
        Ok(())
    }
}

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    /// Emit a supervisor boundary, or report that this operation addresses an
    /// actor instead.
    pub(super) fn emit_supervisor_boundary(
        &self,
        operation: &hew_mir::physical::ActorOperation,
        sources: &[StorageId],
        result: Option<StorageId>,
    ) -> CodegenResult<Option<IntValue<'ctx>>> {
        use hew_mir::physical::ActorOperation;
        match operation {
            ActorOperation::SupervisorSpawn(id) => {
                self.emit_supervisor_spawn(*id, sources, result).map(Some)
            }
            ActorOperation::SupervisorChild {
                supervisor, child, ..
            } => self
                .emit_supervisor_child(*supervisor, *child, false, sources, result)
                .map(Some),
            ActorOperation::SupervisorPoolView {
                supervisor, child, ..
            } => self
                .emit_supervisor_child(*supervisor, *child, true, sources, result)
                .map(Some),
            _ => Ok(None),
        }
    }

    /// Construction cancellation is a drain: retain the operation and config
    /// until the runtime has joined callbacks and rolled back adopted children.
    fn poll_supervisor_construction(
        &self,
        operation: PointerValue<'ctx>,
    ) -> CodegenResult<IntValue<'ctx>> {
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("supervisor construction requires a resumable body".into())
        })?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let poll_fn = get_or_declare_external(
            self.llvm,
            "hew_supervisor_native_spawn_poll",
            self.ctx
                .i32_type()
                .fn_type(&[ptr.into(), self.ctx.i32_type().into()], false),
        )?;
        let poll = self.ctx.append_basic_block(self.value, "supervisor.poll");
        let pending = self
            .ctx
            .append_basic_block(self.value, "supervisor.pending");
        let complete = self
            .ctx
            .append_basic_block(self.value, "supervisor.complete");
        self.builder
            .build_unconditional_branch(poll)
            .llvm_ctx("poll supervisor construction")?;
        self.builder.position_at_end(poll);
        let cancelled = self.state_value("hew_coro_state_is_cancelled", frame.state)?;
        let cancelled = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                cancelled,
                self.ctx.i32_type().const_zero(),
                "supervisor.cancelled",
            )
            .llvm_ctx("inspect construction cancellation")?;
        let cancelling = self
            .ctx
            .append_basic_block(self.value, "supervisor.cancelling");
        let inspect = self
            .ctx
            .append_basic_block(self.value, "supervisor.inspect");
        self.builder
            .build_conditional_branch(cancelled, cancelling, inspect)
            .llvm_ctx("retain the originating cancellation reason")?;
        self.builder.position_at_end(cancelling);
        let reason = self.state_value("hew_coro_state_cancel_code", frame.state)?;
        self.builder
            .build_unconditional_branch(inspect)
            .llvm_ctx("drain cancelled construction")?;
        self.builder.position_at_end(inspect);
        let cancelled = self
            .builder
            .build_phi(self.ctx.i32_type(), "supervisor.cancel.reason")
            .llvm_ctx("select construction cancellation reason")?;
        cancelled.add_incoming(&[
            (&self.ctx.i32_type().const_zero(), poll),
            (&reason, cancelling),
        ]);
        let cancelled = cancelled.as_basic_value();
        let ready = call_value(
            &self.builder,
            poll_fn,
            &[operation.into(), cancelled.into()],
            "supervisor.ready",
        )?
        .into_int_value();
        let ready = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                ready,
                self.ctx.i32_type().const_zero(),
                "supervisor.final",
            )
            .llvm_ctx("check supervisor quiescence")?;
        self.builder
            .build_conditional_branch(ready, complete, pending)
            .llvm_ctx("wait for construction and rollback")?;
        self.builder.position_at_end(pending);
        frame.suspend(self.ctx, &self.builder, poll, frame.invalid_destroy)?;
        self.builder.position_at_end(complete);
        let target = self.target_data;
        let word = self.ctx.ptr_sized_int_type(target, None);
        let take = get_or_declare_external(
            self.llvm,
            "hew_supervisor_native_spawn_take",
            word.fn_type(&[ptr.into(), ptr.into()], false),
        )?;
        let token = call_value(
            &self.builder,
            take,
            &[operation.into(), self.active_fault.into()],
            "supervisor.token",
        )?
        .into_int_value();
        self.free_handle("hew_supervisor_native_spawn_free", operation)?;
        Ok(token)
    }

    fn supervisor(&self, id: SupervisorId) -> CodegenResult<&SemSupervisor> {
        self.module
            .supervisors
            .get(id.0 as usize)
            .filter(|supervisor| supervisor.id == id)
            .ok_or_else(|| CodegenError::FailClosed("missing native supervisor descriptor".into()))
    }

    /// Build the config from the construction arguments and hand the runtime
    /// the declared children in order. A child that cannot be spawned fails
    /// the construction, so a supervisor handle always names a complete tree.
    pub(super) fn emit_supervisor_spawn(
        &self,
        id: SupervisorId,
        sources: &[StorageId],
        result: Option<StorageId>,
    ) -> CodegenResult<IntValue<'ctx>> {
        let supervisor = self.supervisor(id)?;
        let result = result.ok_or_else(|| {
            CodegenError::FailClosed("supervisor spawn lacks its handle output".into())
        })?;
        let is_pool = supervisor
            .children
            .iter()
            .any(|child| child.pool_count.is_some());
        let backoff_children: Vec<usize> = supervisor.backoff_children().collect();
        if sources.len()
            != supervisor.config.len() + 2 * backoff_children.len() + usize::from(is_pool)
        {
            return Err(CodegenError::FailClosed(
                "supervisor spawn does not transfer its exact config".into(),
            ));
        }
        let pool_count = if is_pool {
            self.load(sources[sources.len() - 1], "supervisor.pool.count")?
                .into_int_value()
        } else {
            self.ctx.i64_type().const_zero()
        };
        let backoffs = &sources[supervisor.config.len()..][..2 * backoff_children.len()];
        let sources = &sources[..supervisor.config.len()];
        let child_count = u32::try_from(supervisor.children.len())
            .map_err(|_| CodegenError::FailClosed("supervisor child count exceeds u32".into()))?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let target = self.target_data;
        let word = self.ctx.ptr_sized_int_type(target, None);
        let config_ty = config_type(self.module, self.ctx, supervisor)?;
        let config = if supervisor.config.is_empty() {
            ptr.const_null()
        } else {
            let size = target.get_abi_size(&config_ty.as_basic_type_enum());
            let config =
                super::actor::allocate(self.ctx, self.target_data, self.llvm, &self.builder, size)?;
            for (index, source) in sources.iter().enumerate() {
                let index = u32::try_from(index)
                    .map_err(|_| CodegenError::FailClosed("config field exceeds u32".into()))?;
                let field = self
                    .builder
                    .build_struct_gep(config_ty, config, index, "supervisor.config.field")
                    .llvm_ctx("address config field")?;
                self.builder
                    .build_store(field, self.load(*source, "supervisor.config.value")?)
                    .llvm_ctx("transfer config field")?;
            }
            config
        };
        let drop = self
            .llvm
            .get_function(&symbol(supervisor, "config_drop"))
            .map_or(ptr.const_null(), |function| {
                function.as_global_value().as_pointer_value()
            });
        let children_table = self
            .llvm
            .get_global(&symbol(supervisor, "children"))
            .ok_or_else(|| {
                CodegenError::FailClosed("supervisor lacks its declared child table".into())
            })?
            .as_pointer_value();
        let children = if !backoff_children.is_empty()
            || supervisor.children.iter().any(|child| {
                matches!(
                    child.stop_deadline,
                    hew_mir::physical::SemStopDeadline::Config(_)
                )
            }) {
            let entry_ty = native_child_spec_type(self.ctx);
            let array_ty = entry_ty.array_type(child_count);
            let local = self
                .builder
                .build_alloca(array_ty, "supervisor.children")
                .llvm_ctx("allocate configured child deadlines")?;
            let defaults = self
                .builder
                .build_load(array_ty, children_table, "supervisor.children.defaults")
                .llvm_ctx("read child descriptor defaults")?;
            self.builder
                .build_store(local, defaults)
                .llvm_ctx("copy child descriptor defaults")?;
            let entry = |slot: usize| {
                // SAFETY: the slot is derived from the verified child table.
                unsafe {
                    self.builder.build_in_bounds_gep(
                        array_ty,
                        local,
                        &[
                            self.ctx.i32_type().const_zero(),
                            self.ctx.i32_type().const_int(slot as u64, false),
                        ],
                        "supervisor.child.descriptor",
                    )
                }
                .llvm_ctx("address the configured child")
            };
            for (slot, pair) in backoff_children.iter().zip(backoffs.chunks_exact(2)) {
                let entry = entry(*slot)?;
                for (offset, source) in (0u32..).zip(pair) {
                    let field = self
                        .builder
                        .build_struct_gep(
                            entry_ty,
                            entry,
                            CHILD_BACKOFF_FIELD + offset,
                            "supervisor.child.backoff",
                        )
                        .llvm_ctx("address the child restart delay")?;
                    self.builder
                        .build_store(field, self.load(*source, "supervisor.child.backoff.value")?)
                        .llvm_ctx("set the child restart delay")?;
                }
            }
            for (slot, child) in supervisor.children.iter().enumerate() {
                if let hew_mir::physical::SemStopDeadline::Config(index) = child.stop_deadline {
                    let source = sources.get(index).ok_or_else(|| {
                        CodegenError::FailClosed("stop deadline lost its config parameter".into())
                    })?;
                    let deadline = self.load(*source, "supervisor.stop.deadline")?;
                    let entry = entry(slot)?;
                    let field = self
                        .builder
                        .build_struct_gep(entry_ty, entry, 4, "supervisor.child.stop_ns")
                        .llvm_ctx("address the child stop deadline")?;
                    self.builder
                        .build_store(field, deadline)
                        .llvm_ctx("set the child stop deadline")?;
                }
            }
            local
        } else {
            children_table
        };
        let spawn = get_or_declare_external(
            self.llvm,
            "hew_supervisor_native_spawn_new",
            ptr.fn_type(
                &[
                    self.ctx.i32_type().into(),
                    self.ctx.i32_type().into(),
                    self.ctx.i32_type().into(),
                    ptr.into(),
                    ptr.into(),
                    ptr.into(),
                    word.into(),
                    self.ctx.i64_type().into(),
                    ptr.into(),
                    ptr.into(),
                ],
                false,
            ),
        )?;
        let frame = self.frame.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("supervisor construction requires a resumable body".into())
        })?;
        let wake = get_or_declare_external(
            self.llvm,
            "hew_coro_state_waker",
            ptr.fn_type(&[ptr.into()], false),
        )?;
        let waker = call_value(
            &self.builder,
            wake,
            &[frame.state.into()],
            "supervisor.waker",
        )?;
        let operation = self
            .builder
            .build_call(
                spawn,
                &[
                    self.ctx
                        .i32_type()
                        .const_int(strategy_code(supervisor.strategy), false)
                        .into(),
                    self.ctx
                        .i32_type()
                        .const_int(u64::from(supervisor.max_restarts), false)
                        .into(),
                    self.ctx
                        .i32_type()
                        .const_int(u64::from(supervisor.window_secs), false)
                        .into(),
                    config.into(),
                    drop.into(),
                    children.into(),
                    word.const_int(u64::from(child_count), false).into(),
                    pool_count.into(),
                    waker.into(),
                    self.llvm
                        .get_global("HewLiveCodeOwner")
                        .map_or_else(|| ptr.const_null(), |owner| owner.as_pointer_value())
                        .into(),
                ],
                "supervisor.operation",
            )
            .llvm_ctx("construct the declared supervisor")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed("supervisor construction returned void".into())
            })?
            .into_pointer_value();
        // `new` adopts the config on every path. No cancellation or suspension
        // may leave its transferred source owners available for caller cleanup.
        for source in sources {
            self.clear_owned(*source)?;
        }
        let token = self.poll_supervisor_construction(operation)?;
        self.store(result, token.into())?;
        // The transferred fault owns its category, including deadline and
        // cancellation. Reconstructing a status from the invalid token would
        // erase that category before scope recovery consumes it.
        let fault = self
            .builder
            .build_load(ptr, self.active_fault, "supervisor.fault")
            .llvm_ctx("read the final construction diagnostic")?
            .into_pointer_value();
        self.state_value("hew_fault_code", fault)
    }

    /// `sup.child`: the stable role, minted from the supervisor handle and the
    /// declared slot. Resolution happens at every use, never here.
    pub(super) fn emit_supervisor_child(
        &self,
        id: SupervisorId,
        child: u32,
        is_pool: bool,
        sources: &[StorageId],
        result: Option<StorageId>,
    ) -> CodegenResult<IntValue<'ctx>> {
        let supervisor = self.supervisor(id)?;
        let [source] = sources else {
            return Err(CodegenError::FailClosed(
                "child lookup requires one supervisor handle".into(),
            ));
        };
        let result = result
            .ok_or_else(|| CodegenError::FailClosed("child lookup lacks its role output".into()))?;
        let slot = supervisor
            .slot(child as usize)
            .ok_or_else(|| CodegenError::FailClosed("declared child has no runtime slot".into()))?;
        let token = self.load_supervisor_owner(*source)?;
        if !is_pool {
            let nested = matches!(
                supervisor.children[child as usize].role,
                hew_mir::physical::SemSupervisedRole::Supervisor(_)
            );
            let role = self.mint_actor_role(
                token,
                self.ctx.i32_type().const_int(u64::from(slot), false).into(),
                nested,
            )?;
            self.store(result, role)?;
            return Ok(self.ctx.i32_type().const_zero());
        }
        let role = self.slots[result.0 as usize];
        let mut fields = vec![token.get_type(), self.ctx.i32_type().into()];
        if is_pool {
            fields.push(self.ctx.i64_type().into());
        }
        let role_ty = self.ctx.struct_type(&fields, false);
        let handle = self
            .builder
            .build_struct_gep(role_ty, role, 0, "role.handle")
            .llvm_ctx("address the role's supervisor")?;
        self.builder
            .build_store(handle, token)
            .llvm_ctx("record the role's supervisor")?;
        let key = self
            .builder
            .build_struct_gep(role_ty, role, 1, "role.slot")
            .llvm_ctx("address the role's slot")?;
        self.builder
            .build_store(key, self.ctx.i32_type().const_int(u64::from(slot), false))
            .llvm_ctx("record the role's slot")?;
        if is_pool {
            let count_fn = get_or_declare_external(
                self.llvm,
                "hew_supervisor_native_pool_count",
                self.ctx.i64_type().fn_type(
                    &[token.get_type().into(), self.active_fault.get_type().into()],
                    false,
                ),
            )?;
            let count = self
                .builder
                .build_call(
                    count_fn,
                    &[token.into(), self.active_fault.into()],
                    "pool.count",
                )
                .llvm_ctx("read pool extent")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::FailClosed("pool count returned void".into()))?
                .into_int_value();
            let extent = self
                .builder
                .build_struct_gep(role_ty, role, 2, "pool.extent")
                .llvm_ctx("address pool extent")?;
            self.builder
                .build_store(extent, count)
                .llvm_ctx("record pool extent")?;
            let refused = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    count,
                    self.ctx.i64_type().const_zero(),
                    "pool.refused",
                )
                .llvm_ctx("check pool extent")?;
            return self
                .builder
                .build_int_z_extend(refused, self.ctx.i32_type(), "pool.status")
                .llvm_ctx("pool view status");
        }
        Ok(self.ctx.i32_type().const_zero())
    }

    pub(super) fn emit_supervisor_pool_len(
        &self,
        sources: &[StorageId],
        result: StorageId,
        normal: &hew_mir::physical::PhysicalEdge,
    ) -> CodegenResult<()> {
        let [view] = sources else {
            return Err(CodegenError::FailClosed(
                "pool length requires its view".into(),
            ));
        };
        let view = self.load(*view, "pool.view")?.into_struct_value();
        let count = self
            .builder
            .build_extract_value(view, 2, "pool.count")
            .llvm_ctx("read pool extent")?;
        self.store(result, count)?;
        self.emit_result_edge(Some(result), normal)
    }

    /// `pool[i]` and `pool.get(i)`: one member's role
    /// from the view and the caller's index. The
    /// members occupy consecutive slots from the view's base, so the member is
    /// arithmetic once the index is proved to be one of them.
    pub(super) fn emit_supervisor_pool_member(
        &self,
        option: Option<hew_mir::physical::PhysicalVariantId>,
        sources: &[StorageId],
        result: StorageId,
        normal: &hew_mir::physical::PhysicalEdge,
        failure: Option<&hew_mir::physical::PhysicalEdge>,
    ) -> CodegenResult<()> {
        let [view, index] = sources else {
            return Err(CodegenError::FailClosed(
                "supervisor pool member takes its view and index".into(),
            ));
        };
        let view = self.load(*view, "pool.view")?.into_struct_value();
        let token = self
            .builder
            .build_extract_value(view, 0, "pool.supervisor")
            .llvm_ctx("read the pool's supervisor")?
            .into_int_value();
        let base = self
            .builder
            .build_extract_value(view, 1, "pool.base")
            .llvm_ctx("read the pool's first slot")?
            .into_int_value();
        let index = self.load(*index, "pool.index")?.into_int_value();
        let count = self
            .builder
            .build_extract_value(view, 2, "pool.count")
            .llvm_ctx("read pool extent")?
            .into_int_value();
        // Unsigned comparison also rejects every negative signed index, and the
        // caller's index is never narrowed before the test.
        let in_range = self
            .builder
            .build_int_compare(IntPredicate::ULT, index, count, "pool.index.in.range")
            .llvm_ctx("check pool member bounds")?;
        let present = self
            .ctx
            .append_basic_block(self.value, "pool.member.present");
        let absent = self
            .ctx
            .append_basic_block(self.value, "pool.member.absent");
        self.builder
            .build_conditional_branch(in_range, present, absent)
            .llvm_ctx("select pool member outcome")?;
        self.builder.position_at_end(absent);
        if let Some(option) = option {
            self.write_variant_value(self.slots[result.0 as usize], 1, &[], option)?;
            self.emit_result_edge(Some(result), normal)?;
        } else {
            self.emit_edge(failure.ok_or_else(|| {
                CodegenError::FailClosed("trapping pool member lacks its bounds edge".into())
            })?)?;
        }
        self.builder.position_at_end(present);
        let offset = self
            .builder
            .build_int_truncate(index, self.ctx.i32_type(), "pool.offset")
            .llvm_ctx("narrow the proved pool index")?;
        let slot = self
            .builder
            .build_int_add(base, offset, "pool.slot")
            .llvm_ctx("address the pool member's slot")?;
        let role = self.mint_actor_role(token.into(), slot.into(), false)?;
        if let Some(option) = option {
            self.write_variant_value(self.slots[result.0 as usize], 0, &[role], option)?;
        } else {
            self.store(result, role)?;
        }
        self.emit_result_edge(Some(result), normal)
    }

    fn mint_actor_role(
        &self,
        owner: BasicValueEnum<'ctx>,
        slot: BasicValueEnum<'ctx>,
        nested: bool,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let mint = get_or_declare_external(
            self.llvm,
            "hew_supervisor_native_role_owner",
            owner.into_int_value().get_type().fn_type(
                &[
                    owner.get_type().into(),
                    self.ctx.i32_type().into(),
                    self.ctx.bool_type().into(),
                ],
                false,
            ),
        )?;
        self.builder
            .build_call(
                mint,
                &[
                    owner.into(),
                    slot.into(),
                    self.ctx
                        .bool_type()
                        .const_int(u64::from(nested), false)
                        .into(),
                ],
                "role.token",
            )
            .llvm_ctx("mint the stable role token")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("role token returned void".into()))
    }

    fn load_supervisor_owner(&self, source: StorageId) -> CodegenResult<BasicValueEnum<'ctx>> {
        self.load(source, "role.supervisor")
    }

    pub(super) fn load_actor_target(
        &self,
        id: StorageId,
        name: &str,
    ) -> CodegenResult<IntValue<'ctx>> {
        Ok(self.load(id, name)?.into_int_value())
    }
}

#[cfg(test)]
mod abi_tests {
    use super::super::actor::{c_mirror_mismatch, field_size};
    use super::*;
    use hew_runtime::supervisor::HewNativeChildSpec;
    use std::mem::{align_of, offset_of, size_of};

    fn fields() -> [(usize, usize); 9] {
        [
            (
                offset_of!(HewNativeChildSpec, restart_policy),
                field_size(|spec: &HewNativeChildSpec| &spec.restart_policy),
            ),
            (
                offset_of!(HewNativeChildSpec, role_kind),
                field_size(|spec: &HewNativeChildSpec| &spec.role_kind),
            ),
            (
                offset_of!(HewNativeChildSpec, spawn),
                field_size(|spec: &HewNativeChildSpec| &spec.spawn),
            ),
            (
                offset_of!(HewNativeChildSpec, name),
                field_size(|spec: &HewNativeChildSpec| &spec.name),
            ),
            (
                offset_of!(HewNativeChildSpec, stop_ns),
                field_size(|spec: &HewNativeChildSpec| &spec.stop_ns),
            ),
            (
                offset_of!(HewNativeChildSpec, startup_rank),
                field_size(|spec: &HewNativeChildSpec| &spec.startup_rank),
            ),
            (
                offset_of!(HewNativeChildSpec, dispatch),
                field_size(|spec: &HewNativeChildSpec| &spec.dispatch),
            ),
            (
                offset_of!(HewNativeChildSpec, backoff_initial_ns),
                field_size(|spec: &HewNativeChildSpec| &spec.backoff_initial_ns),
            ),
            (
                offset_of!(HewNativeChildSpec, backoff_maximum_ns),
                field_size(|spec: &HewNativeChildSpec| &spec.backoff_maximum_ns),
            ),
        ]
    }

    #[test]
    fn native_child_spec_matches_the_runtime_c_abi() {
        let ctx = Context::create();
        assert_eq!(
            c_mirror_mismatch(
                native_child_spec_type(&ctx),
                &fields(),
                size_of::<HewNativeChildSpec>(),
                align_of::<HewNativeChildSpec>(),
            ),
            None
        );
    }

    /// The guard sees a reordered mirror: the spawn adapter ahead of the role.
    #[test]
    fn a_reordered_child_spec_mirror_is_caught() {
        let ctx = Context::create();
        let ptr = ctx.ptr_type(AddressSpace::default());
        let reordered = ctx.struct_type(
            &[
                ctx.i32_type().into(),
                ptr.into(),
                ctx.i64_type().into(),
                ctx.i32_type().into(),
                ptr.into(),
                ctx.i32_type().into(),
                ptr.into(),
            ],
            false,
        );
        assert!(c_mirror_mismatch(
            reordered,
            &fields(),
            size_of::<HewNativeChildSpec>(),
            align_of::<HewNativeChildSpec>(),
        )
        .is_some());
    }
}
