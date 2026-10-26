//! Native projection of the owning stage's canonical managed-storage recipes.

use super::*;
use hew_cabi::value::HewTypeOwnershipKind;
use hew_mir::physical::StorageOrigin;
use hew_mir::physical::{ManagedProviderKind as Provider, ManagedStorageShape as Shape};

pub(super) fn bind_frame_roots<'ctx>(
    module: &ModuleEmitter<'ctx, '_>,
    function: &PhysicalFunction,
    builder: &Builder<'ctx>,
    frame: &coro::Frame<'ctx>,
    slots: &[PointerValue<'ctx>],
    flags: &BTreeMap<StorageId, PointerValue<'ctx>>,
    scopes: &BTreeMap<hew_mir::physical::TaskScopeId, PointerValue<'ctx>>,
) -> CodegenResult<()> {
    let mut roots = flags
        .iter()
        .filter(|(id, _)| {
            let storage = &function.storage[id.0 as usize];
            storage.own == OwnKind::Owned
                && function.frame_roots.slots.contains(id)
                && !matches!(
                    storage.origin,
                    StorageOrigin::ActorState { .. } | StorageOrigin::Capture { .. }
                )
        })
        .map(|(id, flag)| {
            let layout = module
                .llvm
                .get_global(&value_layout_symbol(
                    module.module,
                    &function.storage[id.0 as usize].ty,
                )?)
                .ok_or_else(|| {
                    CodegenError::FailClosed("frame root lacks its typed layout".into())
                })?
                .as_pointer_value();
            Ok((layout, slots[id.0 as usize], *flag))
        })
        .collect::<CodegenResult<Vec<_>>>()?;
    if !scopes.is_empty() {
        let scope_layout = module
            .llvm
            .get_global("__hew_budget_task_scope")
            .ok_or_else(|| {
                CodegenError::FailClosed("lexical scope root lacks its descriptor".into())
            })?
            .as_pointer_value();
        let present = module
            .llvm
            .get_global("__hew_budget_root_present")
            .ok_or_else(|| {
                CodegenError::FailClosed("scope root presence constant is absent".into())
            })?
            .as_pointer_value();
        roots.extend(scopes.values().map(|slot| (scope_layout, *slot, present)));
    }
    frame.bind_root_entries(module.ctx, &module.llvm, builder, &roots)?;
    Ok(())
}

/// A protocol handle exposes only its existing typed provider descendants.
pub(super) fn operation_layout<'ctx>(
    ctx: &'ctx Context,
    llvm: &Module<'ctx>,
    visit: &str,
) -> CodegenResult<PointerValue<'ctx>> {
    let name = format!("__hew_budget_operation_{visit}");
    if let Some(layout) = llvm.get_global(&name) {
        return Ok(layout.as_pointer_value());
    }
    let pointer = ctx.ptr_type(AddressSpace::default());
    let callback = get_or_declare_external(
        llvm,
        visit,
        ctx.i32_type().fn_type(&[pointer.into(); 2], false),
    )?;
    let managed_ty = ctx.struct_type(&[ctx.i64_type().into(), pointer.into()], false);
    let managed = llvm.add_global(managed_ty, None, &format!("{name}_managed"));
    managed.set_linkage(Linkage::Internal);
    managed.set_constant(true);
    managed.set_initializer(&managed_ty.const_named_struct(&[
        ctx.i64_type().const_zero().into(),
        callback.as_global_value().as_pointer_value().into(),
    ]));
    let data_layout = llvm.get_data_layout();
    let target = TargetData::create(
        data_layout
            .as_str()
            .to_str()
            .map_err(|_| CodegenError::FailClosed("invalid target data layout".into()))?,
    );
    let size = ctx.ptr_sized_int_type(&target, None);
    let layout_ty = value_descriptor_type(ctx, &target);
    let layout = llvm.add_global(layout_ty, None, &name);
    layout.set_linkage(Linkage::Internal);
    layout.set_constant(true);
    layout.set_initializer(
        &layout_ty.const_named_struct(&[
            size.const_int(u64::from(target.get_pointer_byte_size(None)), false)
                .into(),
            size.const_int(u64::from(target.get_abi_alignment(&pointer)), false)
                .into(),
            ctx.i8_type()
                .const_int(HewTypeOwnershipKind::Plain as u64, false)
                .into(),
            pointer.const_null().into(),
            pointer.const_null().into(),
            pointer.const_null().into(),
            pointer.const_null().into(),
            managed.as_pointer_value().into(),
        ]),
    );
    Ok(layout.as_pointer_value())
}

/// A cleanup cursor retains an allocation after its children begin disposal.
pub(super) fn flat_backing_layout<'ctx>(
    ctx: &'ctx Context,
    llvm: &Module<'ctx>,
    bytes: u64,
) -> CodegenResult<PointerValue<'ctx>> {
    let name = format!("__hew_budget_flat_{bytes}");
    if llvm.get_function(&name).is_none() {
        let pointer = ctx.ptr_type(AddressSpace::default());
        let function = llvm.add_function(
            &name,
            ctx.i32_type().fn_type(&[pointer.into(); 2], false),
            Some(Linkage::Internal),
        );
        let builder = ctx.create_builder();
        builder.position_at_end(ctx.append_basic_block(function, "entry"));
        let identity = builder
            .build_load(
                pointer,
                function.get_nth_param(0).unwrap().into_pointer_value(),
                "backing.identity",
            )
            .llvm_ctx("read retained cleanup allocation")?;
        let visit = get_or_declare_external_widened(
            llvm,
            "hew_managed_visit_backing",
            ctx.i32_type().fn_type(
                &[
                    pointer.into(),
                    pointer.into(),
                    ctx.i64_type().into(),
                    ctx.i8_type().into(),
                ],
                false,
            ),
            &[(3, Widen::Zero)],
        )?;
        let status = suspend::call_value(
            &builder,
            visit,
            &[
                function.get_nth_param(1).unwrap().into(),
                identity.into(),
                ctx.i64_type().const_int(bytes, false).into(),
                ctx.i8_type().const_zero().into(),
            ],
            "backing.status",
        )?
        .into_int_value();
        let skip = builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                ctx.i32_type().const_int(1, false),
                "backing.skip",
            )
            .llvm_ctx("inspect retained backing")?;
        let result = builder
            .build_select(skip, ctx.i32_type().const_zero(), status, "backing.result")
            .llvm_ctx("normalize already retained backing")?;
        builder
            .build_return(Some(&result))
            .llvm_ctx("finish flat backing visit")?;
    }
    operation_layout(ctx, llvm, &name)
}

pub(super) fn value_layout_symbol(
    module: &PhysicalModule,
    ty: &ResolvedTy,
) -> CodegenResult<String> {
    let index = module
        .managed_types
        .keys()
        .position(|candidate| candidate == ty)
        .ok_or_else(|| {
            CodegenError::FailClosed(format!("managed value has no owning type row: {ty:?}"))
        })?;
    Ok(format!("__hew_budget_value_{index}"))
}

impl<'ctx> FunctionEmitter<'_, 'ctx> {
    pub(super) fn managed_value_layout(
        &self,
        ty: &ResolvedTy,
    ) -> CodegenResult<PointerValue<'ctx>> {
        self.llvm
            .get_global(&value_layout_symbol(self.module, ty)?)
            .map(|global| global.as_pointer_value())
            .ok_or_else(|| CodegenError::FailClosed("managed value layout is not emitted".into()))
    }
}

impl<'ctx> ModuleEmitter<'ctx, '_> {
    pub(super) fn emit_actor_message_managed(
        &self,
        actor: &hew_mir::physical::SemActor,
        handler: &hew_mir::physical::SemActorHandler,
        structure: inkwell::types::StructType<'ctx>,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let name = format!(
            "hew.actor.{}.message.{}.managed",
            actor.id.0, handler.message_id
        );
        let function = self.llvm.add_function(
            &format!("{name}.visit"),
            self.ctx.i32_type().fn_type(&[pointer.into(); 2], false),
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(function, "entry"));
        let source = function.get_nth_param(0).unwrap().into_pointer_value();
        let visitor = function.get_nth_param(1).unwrap().into_pointer_value();
        let visit = self.ctx.append_basic_block(function, "message.fields");
        let done = self.ctx.append_basic_block(function, "message.done");
        if self.module.actor_message_tracks_occupancy(handler) {
            let active = builder
                .build_load(self.ctx.i8_type(), source, "message.active")
                .llvm_ctx("read canonical request ownership")?
                .into_int_value();
            let live = builder
                .build_int_compare(
                    IntPredicate::NE,
                    active,
                    self.ctx.i8_type().const_zero(),
                    "message.live",
                )
                .llvm_ctx("test retained request fields")?;
            builder
                .build_conditional_branch(live, visit, done)
                .llvm_ctx("visit initialized request payload")?;
        } else {
            builder
                .build_unconditional_branch(visit)
                .llvm_ctx("visit untracked request payload")?;
        }
        builder.position_at_end(visit);
        let values = ValueEmitter {
            module: self.module,
            ctx: self.ctx,
            llvm: &self.llvm,
            builder: &builder,
            value: function,
            fault_sink: None,
        };
        let mut bytes = 0_u64;
        for (index, ty) in handler.params.iter().enumerate() {
            let row = self.module.managed_types.get(ty).ok_or_else(|| {
                CodegenError::FailClosed("request field lacks canonical managed row".into())
            })?;
            bytes = bytes.checked_add(row.slot_bytes).ok_or_else(|| {
                CodegenError::FailClosed("request payload width overflows".into())
            })?;
            if let Some(callback) = self.managed_visit(ty)? {
                let slot = builder
                    .build_struct_gep(structure, source, (index + 1) as u32, "message.field")
                    .llvm_ctx("address initialized request field")?;
                self.managed_child(&values, callback, slot, visitor)?;
            }
        }
        builder
            .build_unconditional_branch(done)
            .llvm_ctx("finish retained request fields")?;
        builder.position_at_end(done);
        builder
            .build_return(Some(&self.ctx.i32_type().const_zero()))
            .llvm_ctx("complete typed request visit")?;
        let descriptor_ty = self
            .ctx
            .struct_type(&[self.ctx.i64_type().into(), pointer.into()], false);
        let descriptor = self.llvm.add_global(descriptor_ty, None, &name);
        descriptor.set_linkage(Linkage::Internal);
        descriptor.set_constant(true);
        descriptor.set_initializer(&descriptor_ty.const_named_struct(&[
            self.ctx.i64_type().const_int(bytes, false).into(),
            function.as_global_value().as_pointer_value().into(),
        ]));
        Ok(descriptor.as_pointer_value())
    }

    pub(super) fn emit_actor_state_managed(
        &self,
        actor: &hew_mir::physical::SemActor,
        layout: &PhysicalLayout,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let function = self.llvm.add_function(
            &format!("hew.actor.{}.state_managed_visit", actor.id.0),
            self.ctx.i32_type().fn_type(&[pointer.into(); 2], false),
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(function, "entry"));
        let state = function.get_nth_param(0).unwrap().into_pointer_value();
        let visitor = function.get_nth_param(1).unwrap().into_pointer_value();
        let structure = llvm_type(self.ctx, &layout.repr)?.into_struct_type();
        let values = ValueEmitter {
            module: self.module,
            ctx: self.ctx,
            llvm: &self.llvm,
            builder: &builder,
            value: function,
            fault_sink: None,
        };
        let mut bytes = 0_u64;
        for (index, field) in actor.fields.iter().enumerate() {
            let row = self.module.managed_types.get(&field.ty).ok_or_else(|| {
                CodegenError::FailClosed("actor field lacks canonical managed width".into())
            })?;
            bytes = bytes
                .checked_add(row.slot_bytes)
                .ok_or_else(|| CodegenError::FailClosed("actor state payload overflows".into()))?;
            let Some(callback) = self.managed_visit(&field.ty)? else {
                continue;
            };
            let index = u32::try_from(index)
                .map_err(|_| CodegenError::FailClosed("too many actor fields".into()))?;
            let flag = actor::state_field_initialized(self.ctx, &builder, state, layout, index)?;
            let initialized = builder
                .build_load(self.ctx.bool_type(), flag, "state.managed.initialized")
                .llvm_ctx("read canonical state occupancy")?
                .into_int_value();
            let visit = self.ctx.append_basic_block(function, "state.managed.live");
            let next = self.ctx.append_basic_block(function, "state.managed.next");
            builder
                .build_conditional_branch(initialized, visit, next)
                .llvm_ctx("visit only initialized state fields")?;
            builder.position_at_end(visit);
            let slot = builder
                .build_struct_gep(structure, state, index, "state.managed.field")
                .llvm_ctx("address live actor field")?;
            self.managed_child(&values, callback, slot, visitor)?;
            builder
                .build_unconditional_branch(next)
                .llvm_ctx("finish actor managed field")?;
            builder.position_at_end(next);
        }
        builder
            .build_return(Some(&self.ctx.i32_type().const_zero()))
            .llvm_ctx("finish actor managed state")?;
        let descriptor_ty = self
            .ctx
            .struct_type(&[self.ctx.i64_type().into(), pointer.into()], false);
        let descriptor = self.llvm.add_global(
            descriptor_ty,
            None,
            &format!("hew.actor.{}.state_managed", actor.id.0),
        );
        descriptor.set_linkage(Linkage::Internal);
        descriptor.set_constant(true);
        descriptor.set_initializer(&descriptor_ty.const_named_struct(&[
            self.ctx.i64_type().const_int(bytes, false).into(),
            function.as_global_value().as_pointer_value().into(),
        ]));
        Ok(descriptor.as_pointer_value())
    }

    fn managed_index(&self, ty: &ResolvedTy) -> CodegenResult<usize> {
        self.module
            .managed_types
            .keys()
            .position(|candidate| candidate == ty)
            .ok_or_else(|| {
                CodegenError::FailClosed(format!("managed type has no owning descriptor: {ty:?}"))
            })
    }

    pub(super) fn managed_descriptor(&self, ty: &ResolvedTy) -> CodegenResult<PointerValue<'ctx>> {
        let name = format!("__hew_managed_type_{}", self.managed_index(ty)?);
        self.llvm
            .get_global(&name)
            .map(|global| global.as_pointer_value())
            .ok_or_else(|| {
                CodegenError::FailClosed("managed descriptor declaration is missing".into())
            })
    }

    fn managed_visit(&self, ty: &ResolvedTy) -> CodegenResult<Option<FunctionValue<'ctx>>> {
        Ok(self
            .llvm
            .get_function(&format!("__hew_managed_visit_{}", self.managed_index(ty)?)))
    }

    pub(super) fn emit_managed_descriptors(&self) -> CodegenResult<()> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let descriptor = self
            .ctx
            .struct_type(&[self.ctx.i64_type().into(), pointer.into()], false);
        let callback = self.ctx.i32_type().fn_type(&[pointer.into(); 2], false);
        let scope_visit =
            get_or_declare_external(&self.llvm, "hew_managed_visit_task_scope", callback)?;
        let scope_managed = self
            .llvm
            .add_global(descriptor, None, "__hew_budget_scope_managed");
        scope_managed.set_linkage(Linkage::Internal);
        scope_managed.set_constant(true);
        scope_managed.set_initializer(&descriptor.const_named_struct(&[
            self.ctx.i64_type().const_zero().into(),
            scope_visit.as_global_value().as_pointer_value().into(),
        ]));
        let target = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let layout_ty = value_descriptor_type(self.ctx, &target);
        let scope_layout = self
            .llvm
            .add_global(layout_ty, None, "__hew_budget_task_scope");
        scope_layout.set_linkage(Linkage::Internal);
        scope_layout.set_constant(true);
        scope_layout.set_initializer(
            &layout_ty.const_named_struct(&[
                size_ty
                    .const_int(u64::from(target.get_pointer_byte_size(None)), false)
                    .into(),
                size_ty
                    .const_int(u64::from(target.get_abi_alignment(&pointer)), false)
                    .into(),
                self.ctx
                    .i8_type()
                    .const_int(HewTypeOwnershipKind::Plain as u64, false)
                    .into(),
                pointer.const_null().into(),
                pointer.const_null().into(),
                pointer.const_null().into(),
                pointer.const_null().into(),
                scope_managed.as_pointer_value().into(),
            ]),
        );
        let present = self
            .llvm
            .add_global(self.ctx.i8_type(), None, "__hew_budget_root_present");
        present.set_linkage(Linkage::Internal);
        present.set_constant(true);
        present.set_initializer(&self.ctx.i8_type().const_int(1, false));
        // All declarations precede bodies: recursive values reference the same
        // typed rows rather than reconstructing their descendants in codegen.
        for (index, row) in self.module.managed_types.values().enumerate() {
            let leaf = !row.shape.has_managed_children();
            let visit = if leaf {
                pointer.const_null()
            } else {
                self.llvm
                    .add_function(
                        &format!("__hew_managed_visit_{index}"),
                        callback,
                        Some(Linkage::Internal),
                    )
                    .as_global_value()
                    .as_pointer_value()
            };
            let global =
                self.llvm
                    .add_global(descriptor, None, &format!("__hew_managed_type_{index}"));
            global.set_linkage(Linkage::Internal);
            global.set_constant(true);
            global.set_initializer(&descriptor.const_named_struct(&[
                self.ctx.i64_type().const_int(row.slot_bytes, false).into(),
                visit.into(),
            ]));
        }
        for (ty, row) in &self.module.managed_types {
            let Some(function) = self.managed_visit(ty)? else {
                continue;
            };
            let builder = self.ctx.create_builder();
            builder.position_at_end(self.ctx.append_basic_block(function, "entry"));
            let slot = function.get_nth_param(0).unwrap().into_pointer_value();
            let visitor = function.get_nth_param(1).unwrap().into_pointer_value();
            let values = ValueEmitter {
                module: self.module,
                ctx: self.ctx,
                llvm: &self.llvm,
                builder: &builder,
                value: function,
                fault_sink: None,
            };
            let runtime = match &row.shape {
                Shape::String => Some("hew_managed_visit_string"),
                Shape::Bytes => Some("hew_managed_visit_bytes"),
                Shape::Vector { .. } | Shape::Array { .. } | Shape::Slice { .. } => {
                    Some("hew_managed_visit_vector")
                }
                Shape::Map { .. } => Some("hew_managed_visit_map"),
                Shape::Set { .. } => Some("hew_managed_visit_set"),
                Shape::Callable { .. } => Some("hew_managed_visit_callable"),
                Shape::Shared { weak: false, .. } => Some("hew_managed_visit_shared"),
                Shape::Dynamic => Some("hew_managed_visit_dynamic"),
                Shape::Generator { .. } => Some("hew_managed_visit_generator"),
                Shape::Provider {
                    kind: Provider::Task,
                    ..
                } => Some("hew_managed_visit_task"),
                Shape::ActorCall { .. } => Some("hew_managed_visit_actor_call"),
                Shape::Provider {
                    kind: Provider::Stream,
                    ..
                } => Some("hew_managed_visit_stream"),
                Shape::Provider {
                    kind: Provider::Sink,
                    ..
                } => Some("hew_managed_visit_sink"),
                Shape::Provider {
                    kind: Provider::Encoding(hew_types::BuiltinType::JsonValue),
                    ..
                } => Some("hew_managed_visit_json"),
                Shape::Provider {
                    kind: Provider::Encoding(hew_types::BuiltinType::YamlValue),
                    ..
                } => Some("hew_managed_visit_yaml"),
                Shape::Provider {
                    kind: Provider::Encoding(hew_types::BuiltinType::TomlValue),
                    ..
                } => Some("hew_managed_visit_toml"),
                Shape::Provider {
                    kind: Provider::Opaque,
                    ..
                } => Some("hew_managed_visit_opaque"),
                _ => None,
            };
            if let Some(runtime) = runtime {
                let runtime = get_or_declare_external(&self.llvm, runtime, callback)?;
                let status = suspend::call_value(
                    &builder,
                    runtime,
                    &[slot.into(), visitor.into()],
                    "managed.status",
                )?;
                builder
                    .build_return(Some(&status))
                    .llvm_ctx("return managed runtime visit")?;
                continue;
            }
            match &row.shape {
                Shape::Aggregate { fields } => {
                    let layout = self.module.target.layout(ty).ok_or_else(|| {
                        CodegenError::FailClosed("managed aggregate lacks target layout".into())
                    })?;
                    self.managed_fields(&values, slot, layout, fields, visitor)?;
                }
                Shape::Variant {
                    indirect,
                    payload_bytes,
                    cases,
                } => {
                    let layout = self.module.target.variant_layout(ty).ok_or_else(|| {
                        CodegenError::FailClosed("managed variant lacks target layout".into())
                    })?;
                    let object = values.variant_object_ptr(slot, layout)?;
                    if *indirect {
                        self.managed_backing(&values, object, *payload_bytes, true, visitor)?;
                    }
                    let tag = values.load_variant_tag(object, layout)?;
                    let invalid = self.ctx.append_basic_block(function, "invalid");
                    let arms: Vec<_> = cases
                        .iter()
                        .enumerate()
                        .map(|(index, _)| {
                            (
                                tag.get_type().const_int(index as u64, false),
                                self.ctx.append_basic_block(function, "case"),
                            )
                        })
                        .collect();
                    builder
                        .build_switch(tag, invalid, &arms)
                        .llvm_ctx("select managed variant payload")?;
                    for (index, (_, arm)) in arms.iter().enumerate() {
                        builder.position_at_end(*arm);
                        let payload = values.variant_payload_ptr(object, layout)?;
                        self.managed_fields(
                            &values,
                            payload,
                            &layout.variants[index],
                            &cases[index],
                            visitor,
                        )?;
                        builder
                            .build_return(Some(&self.ctx.i32_type().const_zero()))
                            .llvm_ctx("finish managed variant")?;
                    }
                    builder.position_at_end(invalid);
                    builder
                        .build_return(Some(&self.ctx.i32_type().const_int(2, false)))
                        .llvm_ctx("refuse invalid managed tag")?;
                    continue;
                }
                // A provider without a typed payload description is explicitly
                // unavailable at a limited acquisition, never an empty value.
                Shape::Provider { .. } => {
                    builder
                        .build_return(Some(&self.ctx.i32_type().const_int(2, false)))
                        .llvm_ctx("refuse undescribed provider")?;
                    continue;
                }
                _ => {
                    return Err(CodegenError::FailClosed(
                        "managed leaf unexpectedly needs a body".into(),
                    ))
                }
            }
            builder
                .build_return(Some(&self.ctx.i32_type().const_zero()))
                .llvm_ctx("finish managed visit")?;
        }
        for (index, row) in self.module.managed_types.values().enumerate() {
            self.emit_value_descriptor(&format!("__hew_budget_value_{index}"), &row.recipe)?;
        }
        Ok(())
    }

    fn managed_fields(
        &self,
        values: &ValueEmitter<'_, 'ctx>,
        slot: PointerValue<'ctx>,
        layout: &PhysicalLayout,
        fields: &[ResolvedTy],
        visitor: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        if fields.is_empty() {
            return Ok(());
        }
        let structure = llvm_type(self.ctx, &layout.repr)?.into_struct_type();
        for (index, ty) in fields.iter().enumerate() {
            if let Some(callback) = self.managed_visit(ty)? {
                let member = values
                    .builder
                    .build_struct_gep(structure, slot, index as u32, "managed.field")
                    .llvm_ctx("address managed field")?;
                self.managed_child(values, callback, member, visitor)?;
            }
        }
        Ok(())
    }

    fn managed_child(
        &self,
        values: &ValueEmitter<'_, 'ctx>,
        callback: FunctionValue<'ctx>,
        slot: PointerValue<'ctx>,
        visitor: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let status = suspend::call_value(
            values.builder,
            callback,
            &[slot.into(), visitor.into()],
            "managed.child.status",
        )?
        .into_int_value();
        self.managed_continue(values, status, false)
    }

    fn managed_continue(
        &self,
        values: &ValueEmitter<'_, 'ctx>,
        status: IntValue<'ctx>,
        backing: bool,
    ) -> CodegenResult<()> {
        let next = self.ctx.append_basic_block(values.value, "managed.next");
        let stop = self.ctx.append_basic_block(values.value, "managed.stop");
        let descend = values
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                status.get_type().const_zero(),
                "managed.descend",
            )
            .llvm_ctx("test managed visit status")?;
        values
            .builder
            .build_conditional_branch(descend, next, stop)
            .llvm_ctx("continue managed visit")?;
        values.builder.position_at_end(stop);
        let result = if backing {
            let skip = values
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    status,
                    status.get_type().const_int(1, false),
                    "managed.skip",
                )
                .llvm_ctx("test already retained backing")?;
            values
                .builder
                .build_select(
                    skip,
                    status.get_type().const_zero(),
                    status,
                    "managed.result",
                )
                .llvm_ctx("finish retained backing")?
                .into_int_value()
        } else {
            status
        };
        values
            .builder
            .build_return(Some(&result))
            .llvm_ctx("propagate managed visit status")?;
        values.builder.position_at_end(next);
        Ok(())
    }

    fn managed_backing(
        &self,
        values: &ValueEmitter<'_, 'ctx>,
        identity: PointerValue<'ctx>,
        bytes: u64,
        fresh_on_clone: bool,
        visitor: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let callback = get_or_declare_external_widened(
            &self.llvm,
            "hew_managed_visit_backing",
            self.ctx.i32_type().fn_type(
                &[
                    pointer.into(),
                    pointer.into(),
                    self.ctx.i64_type().into(),
                    self.ctx.i8_type().into(),
                ],
                false,
            ),
            &[(3, Widen::Zero)],
        )?;
        let status = suspend::call_value(
            values.builder,
            callback,
            &[
                visitor.into(),
                identity.into(),
                self.ctx.i64_type().const_int(bytes, false).into(),
                self.ctx
                    .i8_type()
                    .const_int(u64::from(fresh_on_clone), false)
                    .into(),
            ],
            "managed.backing.status",
        )?
        .into_int_value();
        self.managed_continue(values, status, true)
    }

    pub(super) fn emit_environment_managed(
        &self,
        name: &str,
        glue: &hew_mir::physical::PhysicalEnvironmentGlue,
        layout: &PhysicalLayout,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let callback = self.llvm.add_function(
            &format!("{name}_managed_visit"),
            self.ctx.i32_type().fn_type(&[pointer.into(); 2], false),
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(callback, "entry"));
        let slot = callback.get_nth_param(0).unwrap().into_pointer_value();
        let visitor = callback.get_nth_param(1).unwrap().into_pointer_value();
        let values = ValueEmitter {
            module: self.module,
            ctx: self.ctx,
            llvm: &self.llvm,
            builder: &builder,
            value: callback,
            fault_sink: None,
        };
        for (index, field) in glue.fields.iter().enumerate() {
            let Some(child) = self.managed_visit(&field.ty)? else {
                continue;
            };
            let live = callable::environment_mask_bit(self.ctx, &builder, slot, index as u32)?;
            let visit = self.ctx.append_basic_block(callback, "capture.visit");
            let next = self.ctx.append_basic_block(callback, "capture.next");
            builder
                .build_conditional_branch(live, visit, next)
                .llvm_ctx("test initialized managed capture")?;
            builder.position_at_end(visit);
            let member =
                callable::environment_field(self.ctx, &builder, layout, slot, index as u32)?;
            self.managed_child(&values, child, member, visitor)?;
            builder
                .build_unconditional_branch(next)
                .llvm_ctx("finish managed capture")?;
            builder.position_at_end(next);
        }
        builder
            .build_return(Some(&self.ctx.i32_type().const_zero()))
            .llvm_ctx("finish managed environment")?;
        let bytes = glue
            .fields
            .iter()
            .try_fold(0_u64, |bytes, field| {
                bytes.checked_add(field.managed_slot_bytes)
            })
            .ok_or_else(|| CodegenError::FailClosed("managed environment width overflow".into()))?;
        let descriptor = self
            .ctx
            .struct_type(&[self.ctx.i64_type().into(), pointer.into()], false);
        let global = self
            .llvm
            .add_global(descriptor, None, &format!("{name}_managed"));
        global.set_linkage(Linkage::Internal);
        global.set_constant(true);
        global.set_initializer(&descriptor.const_named_struct(&[
            self.ctx.i64_type().const_int(bytes, false).into(),
            callback.as_global_value().as_pointer_value().into(),
        ]));
        Ok(global.as_pointer_value())
    }
}
