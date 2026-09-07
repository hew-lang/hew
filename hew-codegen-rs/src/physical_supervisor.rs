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
//! A `ChildRef` is the role `(supervisor handle, slot)`, not an address: every
//! use re-resolves the current incarnation through the supervisor.

use super::*;
use hew_mir::physical::{ParamCarrier, StorageId};
use hew_mir::physical::{
    SemRestartPolicy, SemRestartStrategy, SemSupervisedRole, SemSupervisor, SupervisorId,
};

fn symbol(supervisor: &SemSupervisor, suffix: &str) -> String {
    format!("__hew_supervisor_{}_{suffix}", supervisor.id.0)
}

fn strategy_code(strategy: SemRestartStrategy) -> u64 {
    match strategy {
        SemRestartStrategy::OneForOne => 0,
        SemRestartStrategy::OneForAll => 1,
        SemRestartStrategy::RestForOne => 2,
    }
}

fn restart_code(policy: SemRestartPolicy) -> u64 {
    match policy {
        SemRestartPolicy::Permanent => 0,
        SemRestartPolicy::Transient => 1,
        SemRestartPolicy::Temporary => 2,
    }
}

/// The supervised role a child occupies, refusing what has no native
/// realization yet.
fn actor_role(supervisor: &SemSupervisor, child: usize) -> CodegenResult<()> {
    match supervisor.children[child].role {
        SemSupervisedRole::Actor(_) => Ok(()),
        SemSupervisedRole::Supervisor(_) => Err(CodegenError::FailClosed(
            "a nested supervisor child needs its own supervised-role realization".into(),
        )),
    }
}

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
                actor_role(supervisor, child)?;
                self.emit_supervisor_child_spawn(supervisor, child)?;
            }
            self.emit_supervisor_children(supervisor)?;
        }
        Ok(())
    }

    /// The declared children in construction order: restart policy and the
    /// adapter that produces each incarnation.
    fn emit_supervisor_children(&self, supervisor: &SemSupervisor) -> CodegenResult<()> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let entry_ty = self
            .ctx
            .struct_type(&[self.ctx.i32_type().into(), ptr.into()], false);
        let mut entries = Vec::new();
        for (index, child) in supervisor.children.iter().enumerate() {
            let spawn = self
                .llvm
                .get_function(&symbol(supervisor, &format!("child_{index}_spawn")))
                .ok_or_else(|| {
                    CodegenError::FailClosed("declared child lacks its spawn adapter".into())
                })?;
            entries.push(
                entry_ty.const_named_struct(&[
                    self.ctx
                        .i32_type()
                        .const_int(restart_code(child.restart), false)
                        .into(),
                    spawn.as_global_value().as_pointer_value().into(),
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
            ctx: self.ctx,
            llvm: &self.llvm,
            builder: &builder,
            value: function,
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

    /// `fn(config) -> handle`: one incarnation of a declared child. A faulted
    /// spawn returns the invalid handle, which the runtime charges to the
    /// restart budget instead of publishing a dead-looking child.
    fn emit_supervisor_child_spawn(
        &self,
        supervisor: &SemSupervisor,
        child: usize,
    ) -> CodegenResult<()> {
        let spawn = callable(self.module, supervisor.children[child].spawn)?;
        let config_ty = config_type(self.module, self.ctx, supervisor)?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let target = TargetData::create(&self.module.target.data_layout);
        let word = self.ctx.ptr_sized_int_type(&target, None);
        let function = self.llvm.add_function(
            &symbol(supervisor, &format!("child_{child}_spawn")),
            word.fn_type(&[ptr.into(), ptr.into()], false),
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(function, "entry"));
        let config = function.get_first_param().unwrap().into_pointer_value();
        let fault = function.get_nth_param(1).unwrap().into_pointer_value();
        let handle_layout = spawn.return_layout.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("child spawn callable produces no handle".into())
        })?;
        let handle_ty = llvm_type(self.ctx, &handle_layout.repr)?;
        let handle = builder
            .build_alloca(handle_ty, "spawn.handle")
            .llvm_ctx("allocate child handle")?;
        if spawn.params.len() != supervisor.config.len() {
            return Err(CodegenError::FailClosed(
                "child spawn callable does not take the supervisor config".into(),
            ));
        }
        let mut arguments: Vec<BasicMetadataValueEnum<'ctx>> = Vec::new();
        for (index, parameter) in spawn.params.iter().enumerate() {
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
        let status = builder
            .build_call(self.functions[&spawn.id], &arguments, "spawn.status")
            .llvm_ctx("spawn one declared child incarnation")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("child spawn returned void".into()))?
            .into_int_value();
        let spawned = self.ctx.append_basic_block(function, "spawn.ok");
        let failed = self.ctx.append_basic_block(function, "spawn.failed");
        let ok = builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_zero(),
                "spawn.succeeded",
            )
            .llvm_ctx("check child spawn outcome")?;
        builder
            .build_conditional_branch(ok, spawned, failed)
            .llvm_ctx("publish only a spawned child")?;
        builder.position_at_end(failed);
        // The refusal stays in the caller's slot: construction reports it and
        // a restart releases it.
        builder
            .build_return(Some(&word.const_zero()))
            .llvm_ctx("report a refused child spawn")?;
        builder.position_at_end(spawned);
        let value = builder
            .build_load(handle_ty, handle, "spawn.token")
            .llvm_ctx("load the new child handle")?;
        builder
            .build_return(Some(&value))
            .llvm_ctx("publish the new child handle")?;
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
            ActorOperation::SupervisorChild { supervisor, child } => self
                .emit_supervisor_child(*supervisor, *child, sources, result, false)
                .map(Some),
            ActorOperation::SupervisorAwaitRestart { supervisor, child } => self
                .emit_supervisor_child(*supervisor, *child, sources, result, true)
                .map(Some),
            ActorOperation::SupervisorStop(_) => self.emit_supervisor_stop(sources).map(Some),
            _ => Ok(None),
        }
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
        if sources.len() != supervisor.config.len() {
            return Err(CodegenError::FailClosed(
                "supervisor spawn does not transfer its exact config".into(),
            ));
        }
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let target = TargetData::create(&self.module.target.data_layout);
        let word = self.ctx.ptr_sized_int_type(&target, None);
        let config_ty = config_type(self.module, self.ctx, supervisor)?;
        let config = if supervisor.config.is_empty() {
            ptr.const_null()
        } else {
            let size = target.get_abi_size(&config_ty.as_basic_type_enum());
            let config =
                super::actor::allocate(self.module, self.ctx, self.llvm, &self.builder, size)?;
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
        let children = self
            .llvm
            .get_global(&symbol(supervisor, "children"))
            .ok_or_else(|| {
                CodegenError::FailClosed("supervisor lacks its declared child table".into())
            })?
            .as_pointer_value();
        let spawn = get_or_declare_external(
            self.llvm,
            "hew_supervisor_native_spawn",
            word.fn_type(
                &[
                    self.ctx.i32_type().into(),
                    self.ctx.i32_type().into(),
                    self.ctx.i32_type().into(),
                    ptr.into(),
                    ptr.into(),
                    ptr.into(),
                    word.into(),
                    ptr.into(),
                ],
                false,
            ),
        )?;
        let token = self
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
                    word.const_int(supervisor.children.len() as u64, false)
                        .into(),
                    self.active_fault.into(),
                ],
                "supervisor.token",
            )
            .llvm_ctx("construct the declared supervisor")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed("supervisor construction returned void".into())
            })?
            .into_int_value();
        self.store(result, token.into())?;
        let refused = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                token,
                word.const_zero(),
                "supervisor.refused",
            )
            .llvm_ctx("check supervisor construction")?;
        Ok(self
            .builder
            .build_select(
                refused,
                self.ctx.i32_type().const_int(1, false),
                self.ctx.i32_type().const_zero(),
                "supervisor.status",
            )
            .llvm_ctx("select supervisor construction status")?
            .into_int_value())
    }

    /// `sup.child`: the stable role, minted from the supervisor handle and the
    /// declared slot. Resolution happens at every use, never here.
    pub(super) fn emit_supervisor_child(
        &self,
        id: SupervisorId,
        child: u32,
        sources: &[StorageId],
        result: Option<StorageId>,
        await_restart: bool,
    ) -> CodegenResult<IntValue<'ctx>> {
        let supervisor = self.supervisor(id)?;
        actor_role(supervisor, child as usize)?;
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
        let token = self.load(*source, "role.supervisor")?;
        if await_restart {
            let wait = get_or_declare_external(
                self.llvm,
                "hew_supervisor_native_await_restart",
                self.ctx.void_type().fn_type(
                    &[token.get_type().into(), self.ctx.i32_type().into()],
                    false,
                ),
            )?;
            self.builder
                .build_call(
                    wait,
                    &[
                        token.into(),
                        self.ctx.i32_type().const_int(u64::from(slot), false).into(),
                    ],
                    "",
                )
                .llvm_ctx("wait for the declared child to be live again")?;
        }
        let role = self.slots[result.0 as usize];
        let role_ty = self
            .ctx
            .struct_type(&[token.get_type(), self.ctx.i32_type().into()], false);
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
        Ok(self.ctx.i32_type().const_zero())
    }

    /// `supervisor_stop(sup)`: stop every child, then the supervisor.
    pub(super) fn emit_supervisor_stop(
        &self,
        sources: &[StorageId],
    ) -> CodegenResult<IntValue<'ctx>> {
        let [source] = sources else {
            return Err(CodegenError::FailClosed(
                "supervisor stop requires one handle".into(),
            ));
        };
        let value = self.load(*source, "supervisor.stop.handle")?;
        let stop = get_or_declare_external(
            self.llvm,
            "hew_local_pid_supervisor_stop",
            self.ctx
                .i32_type()
                .fn_type(&[value.get_type().into()], false),
        )?;
        self.builder
            .build_call(stop, &[value.into()], "supervisor.stopped")
            .llvm_ctx("stop the declared supervisor")?;
        Ok(self.ctx.i32_type().const_zero())
    }

    /// Load a message target. A role re-resolves to its current incarnation
    /// here, at the use: a restart is never addressed through a stale handle,
    /// and a spent role loads the invalid handle every boundary reports as
    /// closed rather than guessing it is alive.
    pub(super) fn load_actor_target(
        &self,
        id: StorageId,
        name: &str,
    ) -> CodegenResult<IntValue<'ctx>> {
        let value = self.load(id, name)?;
        if self
            .storage(id)?
            .ty
            .is_builtin(hew_types::BuiltinType::ChildRef)
        {
            return self.resolve_role_handle(value.into_struct_value());
        }
        Ok(value.into_int_value())
    }

    /// Resolve a role to its current incarnation. The tag says what the role
    /// holds — `0` live, `1` restarting, `2` spent — because liveness is
    /// reported here, never guessed from a null handle.
    pub(super) fn resolve_role(
        &self,
        role: inkwell::values::StructValue<'ctx>,
    ) -> CodegenResult<(IntValue<'ctx>, IntValue<'ctx>)> {
        let target = TargetData::create(&self.module.target.data_layout);
        let word = self.ctx.ptr_sized_int_type(&target, None);
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let handle = self
            .builder
            .build_extract_value(role, 0, "role.supervisor")
            .llvm_ctx("read the role's supervisor")?;
        let slot = self
            .builder
            .build_extract_value(role, 1, "role.slot")
            .llvm_ctx("read the role's slot")?;
        let tag = self
            .builder
            .build_alloca(self.ctx.i32_type(), "role.tag")
            .llvm_ctx("allocate the role's occupancy")?;
        let resolve = get_or_declare_external(
            self.llvm,
            "hew_supervisor_native_child",
            word.fn_type(
                &[word.into(), self.ctx.i32_type().into(), ptr.into()],
                false,
            ),
        )?;
        let current = self
            .builder
            .build_call(
                resolve,
                &[handle.into(), slot.into(), tag.into()],
                "role.current",
            )
            .llvm_ctx("resolve the role's current incarnation")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("role resolution returned void".into()))?
            .into_int_value();
        let tag = self
            .builder
            .build_load(self.ctx.i32_type(), tag, "role.occupancy")
            .llvm_ctx("read the role's occupancy")?
            .into_int_value();
        Ok((current, tag))
    }

    /// The current incarnation's handle alone, for a boundary whose own
    /// refusal already reports an absent destination.
    pub(super) fn resolve_role_handle(
        &self,
        role: inkwell::values::StructValue<'ctx>,
    ) -> CodegenResult<IntValue<'ctx>> {
        Ok(self.resolve_role(role)?.0)
    }
}
