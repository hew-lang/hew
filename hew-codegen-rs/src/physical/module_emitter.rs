//! Module assembly: declarations, glue, callables and process entry.

use super::*;

#[cfg(test)]
pub(super) fn build_module<'ctx>(
    ctx: &'ctx Context,
    physical: &PhysicalModule,
    name: &str,
    machine: &TargetMachine,
) -> CodegenResult<Module<'ctx>> {
    build_module_with_host(ctx, physical, name, machine, None, None)
}

pub(super) fn build_module_with_host<'ctx>(
    ctx: &'ctx Context,
    physical: &PhysicalModule,
    name: &str,
    machine: &TargetMachine,
    host: Option<&HostExport<'_>>,
    debug_source: Option<DebugSource<'_>>,
) -> CodegenResult<Module<'ctx>> {
    let triple = machine.get_triple();
    let triple_text = triple.as_str().to_string_lossy();
    if triple_text != physical.target.triple {
        return Err(CodegenError::FailClosed(format!(
            "LLVM machine `{triple_text}` disagrees with physical target `{}`",
            physical.target.triple
        )));
    }
    let target_data = machine.get_target_data();
    let data_layout = target_data.get_data_layout();
    let layout_text = data_layout.as_str().to_string_lossy();
    if layout_text != physical.target.data_layout {
        return Err(CodegenError::FailClosed(
            "LLVM data layout disagrees with verified physical MIR".into(),
        ));
    }
    let llvm = ctx.create_module(name);
    llvm.set_triple(&triple);
    llvm.set_data_layout(&data_layout);
    let debug = debug_source
        .map(|source| debug::DebugEmitter::new(ctx, &llvm, &triple_text, source, &physical.debug));
    let mut emitter = ModuleEmitter {
        ctx,
        module: physical,
        llvm,
        functions: BTreeMap::new(),
        ramps: BTreeMap::new(),
        value_callbacks: BTreeMap::new(),
        debug,
    };
    emitter.declare_functions()?;
    emitter.emit_regex_handles()?;
    emitter.emit_collection_value_descriptors()?;
    emitter.emit_task_descriptors()?;
    emitter.emit_generator_descriptors()?;
    emitter.emit_stream_descriptors()?;
    emitter.emit_environment_descriptors()?;
    emitter.emit_vtables()?;
    emitter.emit_callable_descriptors()?;
    emitter.value_callbacks = emitter.emit_selected_value_callbacks()?;
    emitter.emit_actor_descriptors()?;
    emitter.emit_actor_codec_registration()?;
    emitter.emit_actor_ingress_adapters()?;
    emitter.emit_functions()?;
    emitter.emit_entry()?;
    if let Some(export) = host {
        host::emit(&emitter, export)?;
    }
    // Forward references must resolve before the verifier walks the module.
    if let Some(debug) = &emitter.debug {
        debug.finalize();
    }
    emitter
        .llvm
        .verify()
        .map_err(|error| CodegenError::LlvmVerify(error.to_string()))?;
    if emitter.llvm.get_function("llvm.coro.id").is_some() {
        coro::lower(&emitter.llvm, machine)?;
    }
    Ok(emitter.llvm)
}

/// The module-private array of compiled `*HewRegex` handles, one slot per
/// regex literal, filled in the process entry prologue.
const REGEX_HANDLES: &str = "hew_regex_handles";

/// The single tagged-variant carrier a runtime operation's result needs.
pub(super) fn variant_carrier(action: PhysicalRuntimeAction) -> CodegenResult<PhysicalVariantId> {
    match action.carrier {
        PhysicalRuntimeCarrier::Variant(id) => Ok(id),
        _ => Err(CodegenError::FailClosed(format!(
            "runtime operation `{:?}` has no optional result carrier",
            action.family
        ))),
    }
}

pub(super) fn regex_slot_count(module: &PhysicalModule) -> CodegenResult<Option<u32>> {
    if module.regex_patterns.is_empty() {
        return Ok(None);
    }
    u32::try_from(module.regex_patterns.len())
        .map(Some)
        .map_err(|_| CodegenError::FailClosed("regex literal count exceeds the ABI".into()))
}

pub(super) fn regex_handles<'ctx>(
    llvm: &Module<'ctx>,
) -> CodegenResult<inkwell::values::GlobalValue<'ctx>> {
    llvm.get_global(REGEX_HANDLES).ok_or_else(|| {
        CodegenError::FailClosed("physical module has regex literals but no handle array".into())
    })
}

impl<'ctx> ModuleEmitter<'ctx, '_> {
    pub(super) fn emit_collection_value_descriptors(&self) -> CodegenResult<()> {
        for glue in &self.module.vector_glue {
            self.emit_value_descriptor(&vector_descriptor_symbol(glue.id), &glue.element)?;
        }
        for glue in &self.module.map_glue {
            self.emit_value_descriptor(&map_value_descriptor_symbol(glue.id), &glue.value)?;
        }
        for glue in &self.module.shared_glue {
            self.emit_value_descriptor(
                &format!("__hew_shared_payload_{}_layout", glue.id.0),
                &glue.payload,
            )?;
            let Some(action) = glue.payload.destroy else {
                continue;
            };
            let layout = self.module.target.layout(&glue.payload.ty).ok_or_else(|| {
                CodegenError::FailClosed("shared payload has no target layout".into())
            })?;
            self.emit_value_drop_callback(&shared_payload_drop_symbol(glue.id), layout, action)?;
        }
        Ok(())
    }

    /// The module's compiled-regex handle array, one null slot per literal.
    ///
    /// The slots are filled once in the process entry prologue, before any
    /// user body or actor runs, so a match arm only loads its slot.
    fn emit_regex_handles(&self) -> CodegenResult<()> {
        let Some(count) = regex_slot_count(self.module)? else {
            return Ok(());
        };
        if self.module.entry_callable.is_none() {
            // The slots are filled by the process entry. Without one they
            // would stay null and every arm would silently fail to match.
            return Err(CodegenError::FailClosed(
                "a regex literal needs a process entry to compile its pattern".into(),
            ));
        }
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let global = self
            .llvm
            .add_global(pointer.array_type(count), None, REGEX_HANDLES);
        global.set_linkage(Linkage::Private);
        global.set_initializer(&pointer.const_array(&vec![pointer.const_null(); count as usize]));
        Ok(())
    }

    /// Compile every regex literal into its slot. The checker already proved
    /// each pattern parses, so a null handle is an allocation failure: trap
    /// rather than let `hew_regex_match` read it as "no match".
    fn emit_regex_compilation(&self, builder: &Builder<'ctx>) -> CodegenResult<()> {
        let Some(count) = regex_slot_count(self.module)? else {
            return Ok(());
        };
        let handles = regex_handles(&self.llvm)?;
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let array_ty = pointer.array_type(count);
        let i32_ty = self.ctx.i32_type();
        let i64_ty = self.ctx.i64_type();
        let literal_new = get_or_declare_external(
            &self.llvm,
            "hew_string_literal_new",
            self.ctx
                .void_type()
                .fn_type(&[pointer.into(), i32_ty.into(), pointer.into()], false),
        )?;
        let compile = get_or_declare_external(
            &self.llvm,
            "hew_regex_compile",
            pointer.fn_type(&[pointer.into()], false),
        )?;
        let release = get_or_declare_external(
            &self.llvm,
            "hew_string_drop",
            self.ctx.void_type().fn_type(&[pointer.into()], false),
        )?;
        let text = builder
            .build_alloca(pointer, "regex.pattern")
            .llvm_ctx("allocate the regex pattern slot")?;
        for (index, pattern) in self.module.regex_patterns.iter().enumerate() {
            let len = u32::try_from(pattern.len()).map_err(|_| {
                CodegenError::FailClosed("regex pattern exceeds the u32 literal ABI".into())
            })?;
            let data = self.ctx.const_string(pattern.as_bytes(), false);
            let bytes = self
                .llvm
                .add_global(data.get_type(), None, "regex.pattern.bytes");
            bytes.set_linkage(Linkage::Private);
            bytes.set_constant(true);
            bytes.set_initializer(&data);
            builder
                .build_call(
                    literal_new,
                    &[
                        bytes.as_pointer_value().into(),
                        i32_ty.const_int(u64::from(len), false).into(),
                        text.into(),
                    ],
                    "",
                )
                .llvm_ctx("materialize a regex pattern string")?;
            let pattern_value = builder
                .build_load(pointer, text, "regex.pattern.value")
                .llvm_ctx("load the regex pattern string")?;
            let handle = builder
                .build_call(compile, &[pattern_value.into()], "regex.handle")
                .llvm_ctx("compile a regex literal")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("hew_regex_compile returned no handle".into())
                })?
                .into_pointer_value();
            builder
                .build_call(release, &[pattern_value.into()], "")
                .llvm_ctx("release the regex pattern string")?;
            let slot = unsafe {
                builder
                    .build_gep(
                        array_ty,
                        handles.as_pointer_value(),
                        &[i64_ty.const_zero(), i64_ty.const_int(index as u64, false)],
                        "regex.slot",
                    )
                    .llvm_ctx("address a regex handle slot")?
            };
            builder
                .build_store(slot, handle)
                .llvm_ctx("store a compiled regex handle")?;
        }
        Ok(())
    }

    pub(super) fn emit_value_descriptor(
        &self,
        name: &str,
        recipe: &PhysicalValueRecipe,
    ) -> CodegenResult<()> {
        let value = self.value_descriptor(name, recipe)?;
        let global = self
            .llvm
            .get_global(name)
            .unwrap_or_else(|| self.llvm.add_global(value.get_type(), None, name));
        global.set_linkage(Linkage::Internal);
        global.set_constant(true);
        global.set_initializer(&value);
        Ok(())
    }

    /// All container slots use the same copy/drop ABI and value emitter.
    pub(super) fn value_descriptor(
        &self,
        name: &str,
        recipe: &PhysicalValueRecipe,
    ) -> CodegenResult<inkwell::values::StructValue<'ctx>> {
        let target = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let descriptor_ty = value_descriptor_type(self.ctx, &target);
        let layout =
            self.module.target.layout(&recipe.ty).ok_or_else(|| {
                CodegenError::FailClosed("value recipe has no target layout".into())
            })?;
        let clone = match recipe.clone {
            Some(CloneAction::Bitwise) | None => pointer.const_null(),
            Some(action) => {
                self.emit_value_clone_callback(&format!("{name}_clone"), layout, action)?
            }
        };
        let drop = match recipe.destroy {
            None => pointer.const_null(),
            Some(action) => {
                self.emit_value_drop_callback(&format!("{name}_drop"), layout, action)?
            }
        };
        let ownership = if recipe.own == OwnKind::None {
            HewTypeOwnershipKind::Plain
        } else if recipe.ty == ResolvedTy::String {
            HewTypeOwnershipKind::String
        } else if recipe.ty == ResolvedTy::Bytes {
            HewTypeOwnershipKind::Bytes
        } else {
            HewTypeOwnershipKind::LayoutManaged
        };
        Ok(descriptor_ty.const_named_struct(&[
            size_ty.const_int(layout.size, false).into(),
            size_ty.const_int(u64::from(layout.align), false).into(),
            self.ctx.i8_type().const_int(ownership as u64, false).into(),
            clone.into(),
            drop.into(),
            match recipe.destroy {
                Some(action) if self.module.releases.suspends(action) => {
                    release::callback(self.ctx, &self.llvm, self.module, layout, action)?
                        .as_global_value()
                        .as_pointer_value()
                }
                _ => pointer.const_null(),
            }
            .into(),
        ]))
    }

    pub(super) fn emit_value_clone_callback(
        &self,
        name: &str,
        layout: &PhysicalLayout,
        action: CloneAction,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let function = self.llvm.add_function(
            name,
            self.ctx
                .i32_type()
                .fn_type(&[pointer.into(), pointer.into()], false),
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        let entry = self.ctx.append_basic_block(function, "entry");
        let body = self.ctx.append_basic_block(function, "body");
        builder.position_at_end(entry);
        builder
            .build_unconditional_branch(body)
            .llvm_ctx("enter element clone")?;
        builder.position_at_end(body);
        let emitter = ValueEmitter {
            module: self.module,
            ctx: self.ctx,
            llvm: &self.llvm,
            builder: &builder,
            value: function,
            fault_sink: None,
        };
        let source = function
            .get_nth_param(0)
            .ok_or_else(|| CodegenError::FailClosed("element clone lacks source parameter".into()))?
            .into_pointer_value();
        let destination = function
            .get_nth_param(1)
            .ok_or_else(|| {
                CodegenError::FailClosed("element clone lacks destination parameter".into())
            })?
            .into_pointer_value();
        if action == CloneAction::Callable {
            let clone = callable::callable_clone_function(self.ctx, &self.llvm)?;
            let status = builder
                .build_call(
                    clone,
                    &[source.into(), destination.into()],
                    "callable.clone.status",
                )
                .llvm_ctx("copy nested callable environment")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("callable clone returned no status".into())
                })?;
            builder
                .build_return(Some(&status))
                .llvm_ctx("forward callable value clone status")?;
            return Ok(function.as_global_value().as_pointer_value());
        }
        let original = builder
            .build_load(llvm_type(self.ctx, &layout.repr)?, source, "element.source")
            .llvm_ctx("load borrowed value")?;
        let cloned = emitter.clone_loaded_value(original, layout, action)?;
        builder
            .build_store(destination, cloned)
            .llvm_ctx("initialize copied value")?;
        builder
            .build_return(Some(&self.ctx.i32_type().const_zero()))
            .llvm_ctx("finish element clone")?;
        Ok(function.as_global_value().as_pointer_value())
    }

    pub(super) fn emit_value_drop_callback(
        &self,
        name: &str,
        layout: &PhysicalLayout,
        action: DestroyAction,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let function = self.llvm.add_function(
            name,
            self.ctx.void_type().fn_type(&[pointer.into()], false),
            Some(Linkage::Internal),
        );
        let builder = self.ctx.create_builder();
        let entry = self.ctx.append_basic_block(function, "entry");
        let body = self.ctx.append_basic_block(function, "body");
        builder.position_at_end(entry);
        builder
            .build_unconditional_branch(body)
            .llvm_ctx("enter element destruction")?;
        builder.position_at_end(body);
        let emitter = ValueEmitter {
            module: self.module,
            ctx: self.ctx,
            llvm: &self.llvm,
            builder: &builder,
            value: function,
            fault_sink: None,
        };
        let source = function
            .get_nth_param(0)
            .ok_or_else(|| CodegenError::FailClosed("element drop lacks source parameter".into()))?
            .into_pointer_value();
        let value = builder
            .build_load(llvm_type(self.ctx, &layout.repr)?, source, "element.owner")
            .llvm_ctx("load owned value")?;
        emitter.destroy_loaded_value(value, layout, action)?;
        // Destruction releases the value's children. The caller owns the slot.
        builder
            .build_return(None)
            .llvm_ctx("finish element destruction")?;
        Ok(function.as_global_value().as_pointer_value())
    }

    pub(super) fn declare_functions(&mut self) -> CodegenResult<()> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        for callable in &self.module.callables {
            let mut params = callable
                .params
                .iter()
                .map(|param| match param.carrier {
                    ParamCarrier::Direct => llvm_type(self.ctx, &param.layout.repr).map(Into::into),
                    ParamCarrier::Indirect => Ok(ptr.into()),
                })
                .collect::<CodegenResult<Vec<BasicMetadataTypeEnum<'ctx>>>>()?;
            if callable.return_layout.is_some() {
                params.push(ptr.into());
            }
            params.push(ptr.into());
            let function_type = self.ctx.i32_type().fn_type(&params, false);
            let symbol = emitted_symbol(self.module, callable);
            let function = self.llvm.add_function(&symbol, function_type, None);
            self.functions.insert(callable.id, function);
            if callable.is_resumable {
                params.push(ptr.into());
                let ramp = self.llvm.add_function(
                    &format!("{symbol}$resume"),
                    ptr.fn_type(&params, false),
                    Some(Linkage::Internal),
                );
                self.ramps.insert(callable.id, ramp);
            }
        }
        Ok(())
    }

    fn emit_functions(&self) -> CodegenResult<()> {
        for function in &self.module.functions {
            let callable = callable(self.module, function.callable)?;
            let values = if callable.is_resumable {
                &self.ramps
            } else {
                &self.functions
            };
            let value = *values.get(&function.callable).ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "physical callable {} has no LLVM declaration",
                    function.callable.0
                ))
            })?;
            FunctionEmitter::new(self, function, callable, value)?.emit()?;
            if callable.is_resumable {
                self.emit_sync_wrapper(callable)?;
            }
        }
        Ok(())
    }

    fn emit_entry(&self) -> CodegenResult<()> {
        let Some(entry_id) = self.module.entry_callable else {
            return Ok(());
        };
        let plan = self.module.entry_exit_plan.as_ref().ok_or_else(|| {
            CodegenError::FailClosed("physical executable entry has no typed exit plan".into())
        })?;
        let callable = callable(self.module, entry_id)?;
        if !callable.params.is_empty() {
            return Err(CodegenError::FailClosed(
                "physical process entry must be parameterless".into(),
            ));
        }
        let body = *self.functions.get(&entry_id).ok_or_else(|| {
            CodegenError::FailClosed("physical process entry has no LLVM body".into())
        })?;
        let wrapper = self.llvm.add_function(
            "main",
            self.ctx.i32_type().fn_type(&[], false),
            Some(Linkage::External),
        );
        let entry = self.ctx.append_basic_block(wrapper, "entry");
        let success = self.ctx.append_basic_block(wrapper, "success");
        let failure = self.ctx.append_basic_block(wrapper, "failure");
        let builder = self.ctx.create_builder();
        builder.position_at_end(entry);
        self.emit_process_runtime_start(&builder, wrapper)?;
        self.emit_actor_observe_registration(&builder)?;
        self.emit_regex_compilation(&builder)?;
        let result = if let Some(layout) = &callable.return_layout {
            Some(
                builder
                    .build_alloca(llvm_type(self.ctx, &layout.repr)?, "entry.result")
                    .llvm_ctx("allocate physical entry result")?,
            )
        } else {
            None
        };
        let fault = builder
            .build_alloca(self.ctx.ptr_type(AddressSpace::default()), "entry.fault")
            .llvm_ctx("allocate physical entry fault")?;
        builder
            .build_store(
                fault,
                self.ctx.ptr_type(AddressSpace::default()).const_null(),
            )
            .llvm_ctx("initialize physical entry fault")?;
        let mut args = Vec::<BasicMetadataValueEnum<'ctx>>::new();
        if let Some(result) = result {
            args.push(result.into());
        }
        args.push(fault.into());
        let status = builder
            .build_call(body, &args, "entry.status")
            .llvm_ctx("call physical process entry")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("physical body returned no status".into()))?
            .into_int_value();
        let ok = builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                self.ctx.i32_type().const_zero(),
                "entry.ok",
            )
            .llvm_ctx("compare physical entry status")?;
        builder
            .build_conditional_branch(ok, success, failure)
            .llvm_ctx("branch on physical entry status")?;

        builder.position_at_end(failure);
        let fault_value = builder
            .build_load(
                self.ctx.ptr_type(AddressSpace::default()),
                fault,
                "entry.fault.value",
            )
            .llvm_ctx("load physical entry fault")?
            .into_pointer_value();
        let report = external_fault_report(self.ctx, &self.llvm)?;
        builder
            .build_call(report, &[fault_value.into()], "entry.fault.report")
            .llvm_ctx("report physical entry fault")?;
        let drop = external_fault_drop(self.ctx, &self.llvm)?;
        builder
            .build_call(drop, &[fault_value.into()], "entry.fault.drop")
            .llvm_ctx("drop physical entry fault")?;
        // The status the entry body returned is the fault's private tag, not a
        // process exit code (HEW-SPEC-2026 5.8): an unrecovered trap or panic
        // reports `1` after its typed line reaches stderr. The tag stays
        // internal, and `hew_native_runtime_finish` keeps this `1` because a
        // deliberate non-zero code is never overwritten.
        let failed = self.ctx.i32_type().const_int(1, false);
        let failed = self.emit_process_runtime_finish(&builder, failed)?;
        builder
            .build_return(Some(&failed))
            .llvm_ctx("return physical failure status")?;

        builder.position_at_end(success);
        let exit = emit_entry_success(self.ctx, &builder, result, plan.action.clone(), callable)?;
        let exit = self.emit_process_runtime_finish(&builder, exit)?;
        builder
            .build_return(Some(&exit))
            .llvm_ctx("return physical process status")?;
        if self.module.target.triple.starts_with("wasm32") {
            self.emit_wasi_entry_adapter(wrapper)?;
        }
        Ok(())
    }

    /// Publish the canonical entry adapter the WASI runtime's `_start` calls.
    ///
    /// `main` stays the source-shaped export a freestanding host links against;
    /// `__hew_wasi_main` is the fixed name `hew_runtime`'s `_start` imports, so
    /// a WASI command module reaches the same process entry.
    fn emit_wasi_entry_adapter(&self, process_main: FunctionValue<'ctx>) -> CodegenResult<()> {
        let adapter = self.llvm.add_function(
            "__hew_wasi_main",
            self.ctx.i32_type().fn_type(&[], false),
            Some(Linkage::External),
        );
        let builder = self.ctx.create_builder();
        builder.position_at_end(self.ctx.append_basic_block(adapter, "entry"));
        let status = builder
            .build_call(process_main, &[], "wasi.entry.status")
            .llvm_ctx("call the process entry from the WASI adapter")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed("process entry returned no status for WASI".into())
            })?;
        builder
            .build_return(Some(&status))
            .llvm_ctx("return the WASI process status")?;
        Ok(())
    }
}
