//! `Rc`/`Weak` operations over one shared allocation.
//!
//! The runtime owns the reference counts and the allocation; codegen supplies
//! the payload's layout and, at construction, the payload's own destructor.
//! Every symbol comes from the operation's runtime-call row.

use super::*;
use hew_mir::physical::PhysicalSharedId;
use hew_types::RuntimeCallFamily;

impl FunctionEmitter<'_, '_> {
    /// `Rc.new`, `Rc.get` and `Rc.set`: the three forms that need the payload's
    /// layout. The counting, downgrade and weak-clone forms are ordinary
    /// direct calls over a pointer.
    pub(super) fn emit_shared_call(
        &self,
        family: RuntimeCallFamily,
        glue_id: PhysicalSharedId,
        transfers: &[ArgumentTransfer],
        result: Option<StorageId>,
    ) -> CodegenResult<()> {
        let glue = shared_glue(self.module, glue_id)?;
        let payload_layout = self
            .module
            .target
            .layout(&glue.payload.ty)
            .ok_or_else(|| CodegenError::FailClosed("shared payload has no target layout".into()))?
            .clone();
        let payload_ty = llvm_type(self.ctx, &payload_layout.repr)?;
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let target = TargetData::create(&self.module.target.data_layout);
        let size_ty = self.ctx.ptr_sized_int_type(&target, None);
        let symbol = family.row().symbol;
        match family {
            RuntimeCallFamily::RcNew => {
                let source = transfers
                    .first()
                    .map(argument_source)
                    .ok_or_else(|| CodegenError::FailClosed("`Rc.new` lacks its payload".into()))?;
                // `hew_rc_new` copies the payload out of this slot without
                // touching it, so the operand's own storage is the source.
                let data = self.slots[source.0 as usize];
                let drop_fn = self
                    .llvm
                    .get_function(&shared_payload_drop_symbol(glue_id))
                    .map_or_else(
                        || ptr.const_null(),
                        |f| f.as_global_value().as_pointer_value(),
                    );
                let function = get_or_declare_external(
                    self.llvm,
                    symbol,
                    ptr.fn_type(
                        &[ptr.into(), size_ty.into(), size_ty.into(), ptr.into()],
                        false,
                    ),
                )?;
                self.clear_moved(transfers)?;
                let value = self.runtime_call_value(
                    function,
                    &[
                        data.into(),
                        size_ty.const_int(payload_layout.size, false).into(),
                        size_ty
                            .const_int(u64::from(payload_layout.align), false)
                            .into(),
                        drop_fn.into(),
                    ],
                    "rc.new",
                )?;
                self.store(
                    result.ok_or_else(|| {
                        CodegenError::FailClosed("`Rc.new` lacks its handle storage".into())
                    })?,
                    value,
                )
            }
            RuntimeCallFamily::RcGet => {
                let handle = self.load(
                    transfers.first().map(argument_source).ok_or_else(|| {
                        CodegenError::FailClosed("`Rc.get` lacks its receiver".into())
                    })?,
                    "rc.handle",
                )?;
                let function =
                    get_or_declare_external(self.llvm, symbol, ptr.fn_type(&[ptr.into()], false))?;
                let payload = self
                    .runtime_call_value(function, &[handle.into()], "rc.payload")?
                    .into_pointer_value();
                let loaded = self
                    .builder
                    .build_load(payload_ty, payload, "rc.get")
                    .llvm_ctx("read a shared payload")?;
                let action = glue.payload.clone.ok_or_else(|| {
                    CodegenError::FailClosed("`Rc.get` payload has no copy recipe".into())
                })?;
                let copy =
                    self.value_emitter()
                        .clone_loaded_value(loaded, &payload_layout, action)?;
                self.store(
                    result.ok_or_else(|| {
                        CodegenError::FailClosed("`Rc.get` lacks its result storage".into())
                    })?,
                    copy,
                )
            }
            RuntimeCallFamily::RcSet => {
                let [receiver, replacement] = transfers else {
                    return Err(CodegenError::FailClosed(
                        "`Rc.set` takes a receiver and a replacement".into(),
                    ));
                };
                let handle = self.load(argument_source(receiver), "rc.handle")?;
                // The runtime swaps the replacement in and releases what it
                // displaced, so the replacement rides scratch storage rather
                // than the operand's own slot.
                let staged = self
                    .value_emitter()
                    .entry_scratch(payload_ty, "rc.set.staged")?;
                let value = self.load(argument_source(replacement), "rc.set.value")?;
                self.builder
                    .build_store(staged, value)
                    .llvm_ctx("stage a shared payload replacement")?;
                let function = get_or_declare_external(
                    self.llvm,
                    symbol,
                    self.ctx.void_type().fn_type(&[ptr.into(); 2], false),
                )?;
                self.clear_moved(transfers)?;
                self.runtime_call_void(function, &[handle.into(), staged.into()], "rc.set")?;
                Ok(())
            }
            other => Err(CodegenError::FailClosed(format!(
                "runtime operation `{other:?}` has no shared-allocation emission"
            ))),
        }
    }

    /// `Weak.upgrade`: a null answer is `None`, a payload pointer is `Some`.
    pub(super) fn emit_weak_upgrade(
        &self,
        option: hew_mir::physical::PhysicalVariantId,
        transfers: &[ArgumentTransfer],
        result: StorageId,
        normal: &hew_mir::physical::PhysicalEdge,
    ) -> CodegenResult<()> {
        let ptr = self.ctx.ptr_type(AddressSpace::default());
        let handle = self.load(
            transfers.first().map(argument_source).ok_or_else(|| {
                CodegenError::FailClosed("`Weak.upgrade` lacks its receiver".into())
            })?,
            "weak.handle",
        )?;
        let function = get_or_declare_external(
            self.llvm,
            RuntimeCallFamily::WeakUpgradeRc.row().symbol,
            ptr.fn_type(&[ptr.into()], false),
        )?;
        let upgraded = self
            .runtime_call_value(function, &[handle.into()], "weak.upgrade")?
            .into_pointer_value();
        let live = self
            .builder
            .build_is_not_null(upgraded, "weak.upgrade.live")
            .llvm_ctx("test whether the payload is still alive")?;
        let present = self.ctx.append_basic_block(self.value, "weak.upgrade.some");
        let absent = self.ctx.append_basic_block(self.value, "weak.upgrade.none");
        self.builder
            .build_conditional_branch(live, present, absent)
            .llvm_ctx("select the upgrade outcome")?;
        self.builder.position_at_end(absent);
        self.write_variant_value(self.slots[result.0 as usize], 1, &[], option)?;
        self.emit_result_edge(Some(result), normal)?;
        self.builder.position_at_end(present);
        self.write_variant_value(self.slots[result.0 as usize], 0, &[upgraded.into()], option)?;
        self.emit_result_edge(Some(result), normal)
    }

    /// Release every operand the operation adopts before it is handed over.
    fn clear_moved(&self, transfers: &[ArgumentTransfer]) -> CodegenResult<()> {
        for transfer in transfers {
            if let ArgumentTransfer::Move(source) = transfer {
                self.clear_owned(*source)?;
            }
        }
        Ok(())
    }
}

/// The payload recipe for one shared allocation.
fn shared_glue(
    module: &PhysicalModule,
    id: PhysicalSharedId,
) -> CodegenResult<&hew_mir::physical::PhysicalSharedGlue> {
    module
        .shared_glue
        .get(id.0 as usize)
        .filter(|glue| glue.id == id)
        .ok_or_else(|| CodegenError::FailClosed(format!("unknown physical shared glue {}", id.0)))
}
